// (c) 2015-2017 Vladimír Štill

#include <string>
#include <iostream>
#include <cstdlib>
#include <cstring>
#include <cstddef>
#include <cstdio>
#include <csignal>
#include <functional>
#include <string_view>
#include <chrono>
#include <iomanip>

#include <brick-assert>
#include <brick-fs>
#include <brick-proc>

#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <alloca.h>

using namespace std::literals;

using Timer = std::chrono::steady_clock;
using Time = Timer::time_point;
using Duration = Timer::duration;
using Seconds = std::chrono::seconds;
using Milliseconds = std::chrono::milliseconds;

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
            time_t now = std::chrono::system_clock::to_time_t( std::chrono::system_clock::now() );
            std::cerr << std::put_time( std::localtime( &now ), "[%d-%m-%Y %T] " );

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
    explicit FD( int fd ) : fd( fd ) { }
    explicit operator bool() { return fd >= 0; }
    explicit operator int() { ASSERT_LEQ( 0, fd ); return fd; }
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

int addrsize( const std::string &path ) {
    return offsetof( sockaddr_un, sun_path ) + path.size() + 1;
}

std::string replyError( long xid, std::string msg ) {
    WARN( msg );
    std::stringstream reply;
    reply << "I" << xid << "PnokC" << msg << std::endl;
    return reply.str();
}

// input: a packet in the form "I<xid>Q<id>S<solution>" or
// "HI<xid>Q<id>S<solution>" (where 'H' at the beginning stands for hint mode)
//
// output: packet of for "I<xid>P<res>C<comment>" (where <res> is either 'ok'
// or 'nok')
std::string runExprTest( const std::string &exec, const std::string &qdir, std::string_view packet )
{
    bool hint = false;
    if ( packet.at( 0 ) == 'H' ) {
        hint = true;
        packet.remove_prefix( 1 );
    }

    ASSERT_EQ( packet.at( 0 ), 'I' );
    packet.remove_prefix( 1 );

    auto qpos = packet.find( 'Q' );
    ASSERT_NEQ( qpos, std::string_view::npos );
    long xid = std::stol( std::string( packet.substr( 0, qpos ) ) );
    try {
        packet.remove_prefix( qpos + 1 );

        auto spos = packet.find( 'S' );
        ASSERT_NEQ( spos, std::string_view::npos );
        auto id = packet.substr( 0, spos );

        auto solution = packet.substr( spos + 1 );

        INFO( "received solution with ID "s.append( id ) );

        brick::fs::TempDir wd( "hsExprTestService.XXXXXX",
                               brick::fs::AutoDelete::Yes, brick::fs::UseSystemTemp::Yes );
        std::string studentfile = std::string( wd ) + "/StudentRaw.hs"s; // TODO: wd.str()
        std::string qfile;

        {
            std::ofstream student( studentfile );
            student << solution;
        }
        {
            auto q = qdir + "/"s + std::string( id ) + ".q"s;
            auto qs = { q + ".hs", q + ".cpp", q + ".c", q + ".prolog", q + ".py", q };

            for ( auto qf : qs ) {
                if ( brick::fs::access( qf, R_OK ) )
                    qfile = qf;
            }
            if ( qfile.empty() )
                return replyError( xid, "unknown ID: "s.append( id ) );
        }

        std::vector< std::string > args = { exec, qfile, studentfile, "-I" + qdir };
        if ( hint )
            args.push_back( "--hint" );
        auto r = brick::proc::spawnAndWait( brick::proc::CaptureStdout, args );

        std::stringstream reply;
        reply << "I" << xid << "P" << (r ? "ok" : "nok") << "C" << r.out() << std::endl;
        return reply.str();
    } catch ( std::exception &ex ) {
        return replyError( xid, "EXCEPTION: "s + ex.what() );
    }
}

std::atomic< bool > end;
std::atomic< bool > hotrestart;

void setupSignals() {
    std::signal( SIGPIPE, SIG_IGN );
    std::signal( SIGUSR1, []( int ) -> void {
            end = true;
            hotrestart = true;
        } );
    std::signal( SIGTERM, []( int ) -> void {
            end = true;
        } );
}

struct RQT {
    RQT() : start( Timer::now() ) { }
    ~RQT() {
        long ms = elapsed();
        INFO( "Request handled in " + std::to_string( ms ) + "ms" );
    }

    long elapsed() {
        return std::chrono::duration_cast< Milliseconds >( Timer::now() - start ).count();
    }

    Time start;
};

int start( const std::string &insock ) {
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

    // set timeout, only so that signals can iterrupt the waiting (for the sake
    // of termination and hot restart)
    struct timeval timeo { .tv_sec = 3600 * 24 * 7 * 365, .tv_usec = 0 };
    int r = setsockopt( input, SOL_SOCKET, SO_RCVTIMEO, &timeo, sizeof( timeo ) );
    if ( r != 0 )
        SYSWARN( "setsockopt( SO_RCVTIMEO )" );

    return input;
}

int main( int argc, char **argv ) {
    std::vector< std::string > args{ argv, argv + argc };
    std::vector< std::string > persistArgs;
    auto argit = args.begin();
    std::string insock = "/var/lib/checker/socket";
    std::string qdir = "/var/lib/checker/qdir";
    std::string hsExprTest = "hsExprTest";
    auto hasOpt = [&] { return argit != args.end(); };

    auto self = *argit;
    ++argit;

    setupSignals();
    int input = -1;

    if ( hasOpt() && *argit == "--hotstart" ) {
        ++argit;
        ASSERT( hasOpt() );
        input = std::stoi( *argit );
        INFO( "hot start" );
        ++argit;
    }

    for ( auto *opt : { &insock, &qdir, &hsExprTest } ) {
        if ( hasOpt() ) {
            persistArgs.push_back( *argit );
            *opt = *argit;
            ++argit;
        }
    }

    if ( input < 0 ) {
        input = start( insock );
    }

    INFO( "Listening on " + std::to_string( input ) + ", insock = " + insock +
          ", qdir = " + qdir + ", hsExprTest = " + hsExprTest );

    std::string buffer;
  main_loop:
    while ( !end ) {
        try {
            buffer.resize( MAX_PKG_LEN );
            INFO( "Accepting socket..." );
            FD isSock{ accept( input, nullptr, nullptr ) };
            if ( !isSock ) {
                if ( errno == EBADF )
                    DIE( "invalid socket, terminating" );
                else if ( errno == EINTR )
                    WARN( "accept interrupted by signal" );
                else if ( errno == EAGAIN || errno == EWOULDBLOCK )
                    INFO( "socket timeout" );
                else
                    SYSWARN( "accept failed" );
                continue;
            }

            RQT _;
            INFO( "connection established" );
            int rsize = recv( int( isSock ), &buffer[0], MAX_PKG_LEN, 0 );
            if ( rsize < 0 ) {
                SYSWARN( "recv" );
                continue;
            }
            INFO( "packet received" );
            buffer.resize( rsize );
            auto reply = runExprTest( hsExprTest, qdir, buffer );
            send( int( isSock ), reply.c_str(), reply.size(), 0 ) == int( reply.size() ) || SYSWARN( "send" );
            INFO( "Request handled" );
        } catch ( std::exception &ex ) {
            WARN( "EXCEPTION: "s + ex.what() );
        }
    }
    if ( hotrestart ) {
        std::string hotstart = "--hotstart";
        auto sockstr = std::to_string( input );
        std::vector< char * > passargs { argv[ 0 ], hotstart.data(), sockstr.data() };
        for ( auto &s : persistArgs )
            passargs.push_back( s.data() );
        passargs.push_back( nullptr );
        INFO( "hot restart" );
        execvp( argv[0], passargs.data() );

        WARN( "execvp failed, ignoring hot restart request" );
        end = false;
        hotrestart = false;
        goto main_loop;
    }
    INFO( "terminating" );
}
