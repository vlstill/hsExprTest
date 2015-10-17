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
#include <random>
#include <chrono>
#include <thread>

#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <alloca.h>
#include <poll.h>

using Timer = std::chrono::steady_clock;
using Time = Timer::time_point;
using Duration = Timer::duration;
using Seconds = std::chrono::seconds;

Seconds toSeconds( Duration d ) { return std::chrono::duration_cast< Seconds >( d ); }

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

#define SYSDIE( msg ) DIE( std::string( msg ) + ": " + std::string( std::strerror( errno ) ) + " (" + std::to_string( errno ) + ")" )
#define SYSWARN( msg ) WARN( std::string( msg ) + ": " + std::string( std::strerror( errno ) ) + " (" + std::to_string( errno ) + ")" )

void operator||( bool v, Msg &&f ) { f.inhibit = v; }
void operator&&( bool v, Msg f ) { f.inhibit = !v; }

constexpr int MAX_PKG_LEN = 65536;

// unique_ptr-like holder for file descriptors
struct FD {
    FD() : fd( -1 ) { }
    FD( int fd ) : fd( fd ) { }
    operator int() { return fd; }
    FD &operator=( int fd ) { this->fd = fd; return *this; }
    ~FD() { if ( fd >= 0 ) close( fd ); }
    int fd;
};

struct Service {
    Service() {
        pid = -1;
        ttl = 0;
    }

    std::atomic< long > pid;
    std::string addr;
    std::atomic< long > ttl;
};

std::mt19937 rnd;
std::string serviceExec;
Service services[2];

void ensureServices( int attempt = 0 ) {
    int i = 0;
    bool restarted = false;
    for ( auto &p : services ) {
        ++i;
        if ( attempt == 0 && p.pid > 0 && p.ttl.load( std::memory_order_relaxed ) <= 0 && !restarted ) {
            INFO( "TTL restart for service " + std::to_string( i ) );
            if ( kill( p.pid, SIGKILL ) == 0 ) {
                restarted = true;
                INFO( "killed service " + std::to_string( i ) + "(" + std::to_string( p.pid ) + ")" );
                p.pid = -1;
            } else {
                SYSWARN( "kill" );
            }
        }
        if ( p.pid == -1 ) {
            INFO( "Starting service " + std::to_string( i ) + "..." );
            const char *const argv[] = { "hsExprTestService", p.addr.c_str(), nullptr };
            int pid = fork();
            if ( pid == 0 ) { // child
                std::string log = "hsExprTestService." + std::to_string( i ) + ".log";
                std::freopen( log.c_str(), "a", stdout );
                std::freopen( log.c_str(), "a", stderr );
                execv( serviceExec.c_str(), const_cast< char *const * >( argv ) ) == 0 || SYSDIE( "execv" );
            }
            else if ( pid > 0 ) {
                p.pid = pid;
                p.ttl = std::uniform_real_distribution<>( 1000, 2000 )( rnd );
                INFO( "started, ttl = " + std::to_string( p.ttl ) );
            } else
                SYSWARN( "fork" );
        } else
            p.ttl.fetch_sub( 1, std::memory_order_relaxed );
    }
}

void setupServices() {
    access( serviceExec.c_str(), X_OK ) == 0 || SYSDIE( "service is not executable" );

    INFO( "Setting signal handler" );
    struct sigaction sa;
    sa.sa_handler = []( int ) {
            INFO( "SIGCHLD caught" );
            pid_t pid;
            while ( (pid = waitpid( pid_t( -1 ), nullptr, WNOHANG )) > 0 ) {
                for ( auto &p : services )
                    if ( p.pid == pid )
                        p.pid = -1;
            }
        };
    sa.sa_flags = SA_RESTART | SA_NOCLDSTOP;
    sigaction( SIGCHLD, &sa, 0 ) == 0 || SYSDIE( "sigaction" );

    for ( int i = 0; i < 2; ++i )
        services[ i ].addr = "proxySock." + std::to_string( i + 1 );

    ensureServices();
}

int addrsize( const std::string &path ) {
    return offsetof( sockaddr_un, sun_path ) + path.size() + 1;
}

int niPoll( struct pollfd *fds, nfds_t nfds, int timeout ) {
    int r;
    do {
        r = poll( fds, nfds, timeout );
    } while ( r == -1 && errno == EINTR );
    return r;
}

std::string resend( const std::string &data ) {
    auto start = Timer::now();
    bool killed = false;

    for ( int i = 0; i < 32 && toSeconds( Timer::now() - start ) < Seconds( 3 ); ++i ) {
        ensureServices( i );
        if ( i > 0 )
            std::this_thread::sleep_for( std::chrono::milliseconds( 100 ) );

        FD fds[2];
        struct pollfd socks[2];
        for ( int i = 0; i < 2; ++i ) {
            fds[ i ] = socket( AF_UNIX, SOCK_STREAM | SOCK_NONBLOCK, 0 );
            if ( fds[ i ] < 0 ) {
                SYSWARN( "socket" );
                continue;
            }
            socks[ i ].fd = fds[ i ];

            INFO( "forwarding to " + services[ i ].addr );
            int len = addrsize( services[ i ].addr );
            sockaddr_un *addr = reinterpret_cast< sockaddr_un * >( alloca( len ) );
            addr->sun_family = AF_UNIX;
            std::copy( services[ i ].addr.begin(), services[ i ].addr.end(), addr->sun_path );
            addr->sun_path[ services[ i ].addr.size() ] = 0;
            int con = connect( socks[ i ].fd, reinterpret_cast< sockaddr * >( addr ), len );
            if ( con == -1 && errno != EINPROGRESS ) {
                socks[ i ].fd *= -1; // disable polling
                SYSWARN( "connect" );
                continue;
            }

            socks[ i ].events = POLLOUT | POLLIN;
        }

        if ( socks[ 0 ].fd < 0 && socks[ 1 ].fd < 0 && !killed ) {
            WARN( "No ready sockets, killing services" );
            for ( auto &p : services )
                kill( p.pid, SIGKILL );
            killed = true;
            continue;
        }

        int rpoll = niPoll( socks, 2, 500 );
        if ( rpoll <= 0 ) {
            if ( rpoll < 0 )
                SYSWARN( "poll" );
            else
                WARN( "connect timeout" );
            continue;
        }

        bool written = false;
        for ( auto &s : socks ) {
            if ( (s.revents & POLLOUT) != 0 ) {
                int out = -1;
                unsigned len = sizeof( int );
                getsockopt( s.fd, SOL_SOCKET, SO_ERROR, &out, &len ) == 0 || SYSWARN( "getsockopt" );
                out == 0 || WARN( std::strerror( out ) );
                if ( out == 0 ) {
                    if ( send( s.fd, data.data(), data.size(), 0 ) == int( data.size() ) ) {
                        written = true;
                        s.events ^= POLLOUT; // don't watch for writes any more
                        INFO( "Forwarded message" );
                    } else
                        SYSWARN( "send" );
                }
            }
        }
        if ( !written )
            continue;

        INFO( "Wainting for reply" );
        rpoll = niPoll( socks, 2, 5000 );
        if ( rpoll <= 0 ) {
            if ( rpoll < 0 )
                SYSWARN( "poll" );
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
                    SYSWARN( "recv" );
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
    std::signal( SIGUSR1, []( int ) {
            INFO( "SIGUSR1: setting restart flag" );
            for ( auto &s : services )
                s.ttl = 0;
        } );
}

int main( int argc, char **argv ) {
    std::string insock = argc > 1 ? argv[1] : "/var/lib/checker/socket";
    serviceExec = argc > 2 ? argv[2] : "./hsExprTestService";

    setupSignals();
    setupServices();

    INFO( "Creating socket" );
    int input = socket( AF_UNIX, SOCK_STREAM, 0 );
    input >= 0 || SYSDIE( "socket" );

    INFO( "Binding socket" );
    unlink( insock.c_str() );
    int ilen = addrsize( insock );
    sockaddr_un *inaddr = reinterpret_cast< sockaddr_un * >( alloca( ilen ) );
    inaddr->sun_family = AF_UNIX;
    std::copy( insock.begin(), insock.end(), inaddr->sun_path );
    inaddr->sun_path[ insock.size() ] = 0;
    bind( input, reinterpret_cast< const sockaddr * >( inaddr ), ilen ) == 0 || SYSDIE( "bind" );
    chmod( insock.c_str(), 0666 ) == 0 || SYSDIE( "chmod" );

    INFO( "Setting up socket for listening" );
    listen( input, 1 ) == 0 || SYSDIE( "listen" );

    std::string buffer;
    while ( true ) {
        buffer.resize( MAX_PKG_LEN );
        INFO( "Accepting socket..." );
        FD isSock = accept( input, nullptr, nullptr );
        if ( isSock < 0 ) {
            SYSWARN( "accept" );
            continue;
        }

        INFO( "connection established" );
        int rsize = recv( isSock, &buffer[0], MAX_PKG_LEN, 0 );
        if ( rsize < 0 ) {
            SYSWARN( "recv" );
            continue;
        }
        INFO( "packet received" );
        buffer.resize( rsize );
        auto reply = resend( buffer );
        send( isSock, reply.c_str(), reply.size(), 0 ) == int( reply.size() ) || SYSWARN( "send" );
        INFO( "Request handled" );
    }
}
