// (c) 2015 Vladimír Štill

#include <string>
#include <iostream>
#include <cstdlib>
#include <cstring>
#include <cstddef>
#include <cstdio>
#include <csignal>
#include <atomic>
#include <functional>

#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <alloca.h>
#include <sys/wait.h>
#include <poll.h>

enum class Level { Info, Warning, Error };

struct Msg {
    Msg( Level level, std::string msg ) :
        level( level ), msg( msg )
    { }
    Msg( Level level, std::string msg, const char *file, int line, const char *func ) :
        level( level ), msg( msg ), file( file ), line( line ), func( func )
    { }

    ~Msg() {
        if ( !inhibit ) {
            if ( level == Level::Error )
                std::cerr << "FATAL: ";
            else if ( level == Level::Warning )
                std::cerr << "WARNING: ";
            else
                std::cerr << "INFO: ";
            std::cerr << msg;
            if ( file )
                std::cerr << ", at " << file << ":" << line
                          << " (" << func << ")" << std::endl;
            if ( level == Level::Error )
                std::abort();
        }
    }

    bool inhibit = false;
    const Level level;
    std::string msg;
    const char *file = nullptr;
    const int line = 0;
    const char *func = nullptr;
};

#define DIE( msg ) Msg( Level::Error, msg, __FILE__, __LINE__, __func__ )
#define WARN( msg ) Msg( Level::Warning, msg, __FILE__, __LINE__, __func__ )
#define INFO( msg ) Msg( Level::Info, msg, __FILE__, __LINE__, __func__ )

#define SYSDIE() DIE( std::string( std::strerror( errno ) ) + " (" + std::to_string( errno ) + ")" )
#define SYSWARN() WARN( std::string( std::strerror( errno ) ) + " (" + std::to_string( errno ) + ")" )

void operator||( bool v, Msg &&f ) { f.inhibit = v; }
void operator&&( bool v, Msg f ) { f.inhibit = !v; }

constexpr int MAX_PKG_LEN = 65536;

std::string serviceExec;
std::pair< std::atomic< long >, std::string > services[2];

void ensureServices() {
    int i = 0;
    for ( auto &p : services ) {
        ++i;
        if ( p.first == -1 ) {
            INFO( "Starting service..." );
            const char *const argv[] = { "hsExprTestService", p.second.c_str(), nullptr };
            int pid = fork();
            if ( pid == 0 ) { // child
                std::string log = "hsExprTestService." + std::to_string( i ) + ".log";
                std::freopen( log.c_str(), "a", stdout );
                std::freopen( log.c_str(), "a", stderr );
                execv( serviceExec.c_str(), const_cast< char *const * >( argv ) ) == 0 || SYSDIE();
            }
            else if ( pid > 0 ) {
                INFO( "started" );
                p.first = pid;
            } else
                SYSDIE();
        }
    }
}

void setupServices() {
    access( serviceExec.c_str(), X_OK ) == 0 || SYSDIE();

    INFO( "Setting signal handler" );
    struct sigaction sa;
    sa.sa_handler = []( int ) {
            INFO( "SIGCHLD caught" );
            pid_t pid;
            while ( (pid = waitpid( pid_t( -1 ), nullptr, WNOHANG )) > 0 ) {
                for ( auto &p : services )
                    if ( p.first == pid )
                        p.first = -1;
            }
        };
    sa.sa_flags = SA_RESTART | SA_NOCLDSTOP;
    sigaction( SIGCHLD, &sa, 0 ) == 0 || SYSDIE();

    for ( int i = 0; i < 2; ++i ) {
        services[ i ].first = -1;
        services[ i ].second = "proxySock." + std::to_string( i + 1 );
    }

    ensureServices();
}

int addrsize( const std::string &path ) {
    return offsetof( sockaddr_un, sun_path ) + path.size() + 1;
}

struct Defer {
    template< typename Act >
    Defer( Act act ) : act( act ) { }
    ~Defer() { act(); }
    std::function< void() > act;
};

std::string resend( const std::string &data ) {
    for ( int i = 0; i < 3; ++i ) {
        ensureServices();
        if ( i > 0 )
            usleep( 100 );

        struct pollfd socks[2];
        for ( int i = 0; i < 2; ++i ) {
            socks[ i ].fd = socket( AF_UNIX, SOCK_STREAM | SOCK_NONBLOCK, 0 );
            if ( socks[ i ].fd < 0 ) {
                SYSWARN();
                continue;
            }

            INFO( "forwarding to " + services[ i ].second );
            int len = addrsize( services[ i ].second );
            sockaddr_un *addr = reinterpret_cast< sockaddr_un * >( alloca( len ) );
            addr->sun_family = AF_UNIX;
            std::copy( services[ i ].second.begin(), services[ i ].second.end(), addr->sun_path );
            addr->sun_path[ services[ i ].second.size() ] = 0;
            int con = connect( socks[ i ].fd, reinterpret_cast< sockaddr * >( addr ), len );
            if ( con == -1 && errno != EINPROGRESS ) {
                socks[ i ].fd *= -1; // disable polling
                SYSWARN();
                continue;
            }

            socks[ i ].events = POLLOUT | POLLIN;
        }
        Defer _{ [&] { for ( auto &s : socks ) close( s.fd ); } };

        int rpoll = poll( socks, 2, 500 );
        if ( rpoll <= 0 ) {
            if ( rpoll < 0 )
                SYSWARN();
            else
                WARN( "connect timeout" );
            continue;
        }

        bool written = false;
        for ( auto &s : socks ) {
            if ( (s.revents & POLLOUT) != 0 ) {
                int out = -1;
                unsigned len = sizeof( int );
                getsockopt( s.fd, SOL_SOCKET, SO_ERROR, &out, &len ) == 0 || SYSWARN();
                out == 0 || WARN( std::strerror( out ) );
                if ( out == 0 ) {
                    if ( send( s.fd, data.data(), data.size(), 0 ) == int( data.size() ) ) {
                        written = true;
                        s.events ^= POLLOUT; // don't watch for writes any more
                        INFO( "Forwarded message" );
                    } else
                        SYSWARN();
                }
            }
        }
        if ( !written )
            continue;

        INFO( "Wainting for reply" );
        rpoll = poll( socks, 2, 5000 );
        if ( rpoll <= 0 ) {
            if ( rpoll < 0 )
                SYSWARN();
            else
                WARN( "Timeout while waiting for reply" );
            break; // give up
        }
        for ( auto &s : socks ) {
            if ( (s.revents & POLLIN) != 0 ) {
                std::string buf;
                buf.resize( MAX_PKG_LEN );
                int rsize = recv( s.fd, &buf[0], MAX_PKG_LEN, 0 );
                if ( rsize <= 0 ) {
                    SYSWARN();
                    continue;
                }
                buf.resize( rsize );
                return buf;
            }
        }
    }
    WARN( "Given up on this, replying timeout to IS" );
    return "I0PnokCTimeout";
}

void setupSignals() {
    std::signal( SIGPIPE, SIG_IGN );
}

int main( int argc, char **argv ) {
    std::string insock = argc > 1 ? argv[1] : "/var/lib/checker/socket";
    serviceExec = argc > 2 ? argv[2] : "./hsExprTestService";

    setupSignals();
    setupServices();

    INFO( "Creating socket" );
    int input = socket( AF_UNIX, SOCK_STREAM, 0 );
    input >= 0 || SYSDIE();

    INFO( "Binding socket" );
    unlink( insock.c_str() );
    int ilen = addrsize( insock );
    sockaddr_un *inaddr = reinterpret_cast< sockaddr_un * >( alloca( ilen ) );
    inaddr->sun_family = AF_UNIX;
    std::copy( insock.begin(), insock.end(), inaddr->sun_path );
    inaddr->sun_path[ insock.size() ] = 0;
    bind( input, reinterpret_cast< const sockaddr * >( inaddr ), ilen ) == 0 || SYSDIE();

    INFO( "Setting up socket for listening" );
    listen( input, 1 ) == 0 || SYSDIE();

    std::string buffer;
    while ( true ) {
        buffer.resize( MAX_PKG_LEN );
        INFO( "Accepting socket..." );
        int isSock = accept( input, nullptr, nullptr );
        if ( isSock < 0 ) {
            SYSWARN();
            continue;
        }
        INFO( "connection established" );
        int rsize = recv( isSock, &buffer[0], MAX_PKG_LEN, 0 );
        if ( rsize < 0 ) {
            SYSWARN();
            continue;
        }
        INFO( "packet received" );
        buffer.resize( rsize );
        auto reply = resend( buffer );
        send( isSock, reply.c_str(), reply.size(), 0 ) == int( reply.size() ) || SYSWARN();
        INFO( "Request handled" );
    }
}
