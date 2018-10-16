// (c) 2015-2018 Vladimír Štill

#include "work.hpp"
#include "signalfd.hpp"
#include "config.hpp"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused"
#pragma GCC diagnostic ignored "-Wunused-function"
#pragma GCC diagnostic ignored "-Wignored-attributes"
#include <brick-assert>
#include <brick-fs>
#include <brick-proc>
#pragma GCC diagnostic pop

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
#include <mutex>
#include <condition_variable>
#include <atomic>
#include <array>

#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/acl.h>
#include <pwd.h>

#include <alloca.h>
#include <poll.h>

namespace exprtest {

using namespace std::literals;

using Timer = std::chrono::steady_clock;
using Time = Timer::time_point;
using Duration = Timer::duration;
using Seconds = std::chrono::seconds;
using Milliseconds = std::chrono::milliseconds;

Seconds toSeconds( Duration d ) { return std::chrono::duration_cast< Seconds >( d ); }

enum class Level { Info, Warning, Error };

std::mutex out_mtx;
struct Msg {
    Msg( Level level, std::string msg ) :
        level( level ), msg( msg )
    { }
    Msg( Level level, std::string msg, const char *file, int line, const char *func ) :
        level( level ), msg( msg ), file( file ), line( line ), func( func )
    { }

    ~Msg() {
        if ( !inhibit ) {
            std::lock_guard< std::mutex > _( out_mtx );

            if ( log_time ) {
                time_t now = std::chrono::system_clock::to_time_t( std::chrono::system_clock::now() );
                std::cerr << std::put_time( std::localtime( &now ), "[%d-%m-%Y %T] " );
            }

            if ( level == Level::Error )
                std::cerr << "FATAL: ";
            else if ( level == Level::Warning )
                std::cerr << "WARNING: ";
            else
                std::cerr << "INFO: ";
            std::cerr << msg;
            if ( verbose && file )
                std::cerr << ", at " << file << ":" << line
                          << " (" << func << ")";
            std::cerr << std::endl;
            if ( level == Level::Error )
                std::abort();
        }
    }

    bool inhibit = false, verbose = false;
    const Level level;
    std::string msg;
    const char *file = nullptr;
    const int line = 0;
    const char *func = nullptr;
    static bool log_time;
};

bool Msg::log_time = false;

#define DIE( msg ) Msg( Level::Error, msg, __FILE__, __LINE__, __func__ )
#define WARN( msg ) Msg( Level::Warning, msg, __FILE__, __LINE__, __func__ )
#define INFO( msg ) Msg( Level::Info, msg, __FILE__, __LINE__, __func__ )

#define SYSDIE( msg ) DIE( std::string( msg ) + ": " + std::string( std::strerror( errno ) ) + " (" + std::to_string( errno ) + ")" )
#define SYSWARN( msg ) WARN( std::string( msg ) + ": " + std::string( std::strerror( errno ) ) + " (" + std::to_string( errno ) + ")" )

void operator||( bool v, Msg &&f ) { f.inhibit = v; }
void operator&&( bool v, Msg f ) { f.inhibit = !v; }

constexpr int MAX_PKG_LEN = 65536;

int addrsize( const std::string &path ) {
    return offsetof( sockaddr_un, sun_path ) + path.size() + 1;
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

struct Eval
{
    Eval( const Config &c ) : _config( c ) { }

    static std::string sudousr( std::string course ) { return "rc-"s + course; }

    static std::string replyError( long xid, std::string msg ) {
        WARN( msg );
        std::stringstream reply;
        reply << "I" << xid << "PnokC" << msg << std::endl;
        return reply.str();
    }

    static auto spawnAndWait( bool sudo, std::string usr, std::string wd,
                              std::vector< std::string > args )
    {
        if ( sudo )
            args.insert( args.begin(), { "sudo",  "-n", "-u", sudousr( usr ) } );
        return brick::proc::spawnAndWait( brick::proc::CaptureStdout
                                          | brick::proc::WorkDir( wd ),
                                          args );
    }

    // input: a packet in the form "I<xid>Q<id>S<solution>" or
    // "HI<xid>Q<id>S<solution>" (where 'H' at the beginning stands for hint mode)
    //
    // output: packet of for "I<xid>P<res>C<comment>" (where <res> is either 'ok'
    // or 'nok')
    std::string runChecker( std::string_view packet ) const
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
            INFO( "received solution with ID "s.append( id ) );

            auto replyUnknown = [&]{ return replyError( xid, "unknown ID: "s.append( id ) ); };
            if ( id.find( ".." ) != std::string_view::npos ) {
                WARN( "Possible attempt to get out of qdir" );
                return replyUnknown();
            }

            std::string course = "ib015";
            if ( auto c = id.find( ':' ); c != std::string_view::npos ) {
                course = id.substr( 0, c );
                id = id.substr( c + 1 );
            }

            auto solution = packet.substr( spos + 1 );

            brick::fs::TempDir wd( "hsExprTestService.XXXXXX",
                                   brick::fs::AutoDelete::Yes, brick::fs::UseSystemTemp::Yes );

            if ( _config[ course ].isolation )
            {
                auto usrname = sudousr( course );
                auto passwd = getpwnam( usrname.c_str() );
                ASSERT( passwd != nullptr && "getpwnam" );
                auto checker = geteuid();

                // normal ACL
                acl_t acl = acl_get_file( wd.path.c_str(), ACL_TYPE_ACCESS );
                ASSERT( acl != 0 && "acl_get_file" );

                // grant SUDO user rwx
                acl_entry_t usr_perms;
                auto r = acl_create_entry( &acl, &usr_perms );
                ASSERT( r == 0 && "acl_create_entry" );

                r = acl_set_tag_type( usr_perms, ACL_USER );
                ASSERT( r == 0 && "acl_set_tag_type" );
                r = acl_set_qualifier( usr_perms, &passwd->pw_uid );
                ASSERT( r == 0 && "acl_set_qualifier" );
                acl_permset_t usr_permset;
                r = acl_get_permset( usr_perms, &usr_permset );
                ASSERT( r == 0 && "acl_get_permset" );
                r = acl_add_perm( usr_permset, ACL_READ | ACL_WRITE | ACL_EXECUTE );
                ASSERT( r == 0 && "acl_add_perm" );

                r = acl_calc_mask( &acl );
                ASSERT( r == 0 && "acl_add_perm" );

                acl_set_file( wd.path.c_str(), ACL_TYPE_ACCESS, acl );

                // ACL for default entry - based on normal
                acl_t defacl = acl_dup( acl );
                ASSERT( defacl != 0 && "acl_dup" );

                // add RWX for checker so we can delete everything
                acl_entry_t checker_rwx;
                r = acl_create_entry( &defacl, &checker_rwx );
                ASSERT( r == 0 && "acl_create_entry" );

                r = acl_set_tag_type( checker_rwx, ACL_USER );
                ASSERT( r == 0 && "acl_set_tag_type" );
                r = acl_set_qualifier( checker_rwx, &checker );
                ASSERT( r == 0 && "acl_set_qualifier" );
                acl_permset_t checker_permset;
                r = acl_get_permset( checker_rwx, &checker_permset );
                ASSERT( r == 0 && "acl_get_permset" );
                r = acl_add_perm( checker_permset, ACL_READ | ACL_WRITE | ACL_EXECUTE );
                ASSERT( r == 0 && "acl_add_perm" );

                acl_set_file( wd.path.c_str(), ACL_TYPE_DEFAULT, defacl );

                acl_free( acl );
                acl_free( defacl );
            }

            std::string studentfile = wd.path + "/StudentRaw";
            std::string qfile;

            {
                auto q = _config.qdir( course ) + "/"s + std::string( id ) + ".q"s;
                auto exts = { ".hs", ".py" };

                for ( auto e : exts ) {
                    auto name = q + e;
                    if ( brick::fs::access( name, R_OK ) ) {
                        qfile = name;
                        studentfile += e;
                        break;
                    }
                }
                if ( qfile.empty() ) {
                    INFO( "Unknown ID" );
                    return replyUnknown();
                }
            }
            {
                std::ofstream student( studentfile );
                student << solution;
            }

            std::vector< std::string > args;
            {
                std::stringstream ss( _config[ course ].checker );
                std::string cmdpart;
                while ( std::getline( ss, cmdpart, ' ' ) ) {
                    args.push_back( cmdpart );
                }
                args.insert( args.end(), { qfile,
                                           studentfile,
                                           "-I" + _config.qdir( course )
                                          } );
            }
            if ( hint && !_config[ course ].hint )
                throw std::runtime_error( "unauthorized" );
            if ( hint )
                args.push_back( "--hint" );
            auto r = spawnAndWait( _config[course].isolation, course, wd.path, args );

            std::stringstream reply;
            reply << "I" << xid << "P" << (r ? "ok" : "nok") << "C"
                  << "<span style=\"font-family: monospace\">";
            for ( auto c : r.out() ) {
                switch( c ) {
                    case '&':  reply << "&amp;";    break;
                    case '\"': reply << "&quot;";   break;
                    case '\'': reply << "&apos;";   break;
                    case '<':  reply << "&lt;";     break;
                    case '>':  reply << "&gt;";     break;
                    default:   reply << c;          break;
                }
            }
            reply << "</span>" << std::endl;
            return reply.str();
        } catch ( std::exception &ex ) {
            return replyError( xid, "EXCEPTION: "s + ex.what() );
        }
    }

    void loop( WorkSet< FD > &work )
    {
        _buffer.resize( MAX_PKG_LEN );
        try {
            auto isSock = work.pop();
            if ( !isSock ) {
                return;
            }

            RQT _;

            int rsize = recv( isSock->fd, &_buffer[0], MAX_PKG_LEN, 0 );

            if ( rsize < 0 ) {
                SYSWARN( "recv" );
            }
            else {
                _buffer.resize( rsize );
                auto reply = runChecker( _buffer );
                send( isSock->fd, reply.c_str(), reply.size(), 0 ) == int( reply.size() ) || SYSWARN( "send" );
            }
        } catch ( std::exception &ex ) {
            WARN( "EXCEPTION: "s + ex.what() );
        }
    }

    std::string _buffer;
    const Config &_config;
};

int start( const Config &config ) {
    INFO( "Creating socket" );
    int input = socket( AF_UNIX, SOCK_STREAM, 0 );
    input >= 0 || SYSDIE( "socket" );

    INFO( "Binding socket" );
    unlink( config.socket.c_str() );
    int ilen = addrsize( config.socket );
    sockaddr_un *inaddr = reinterpret_cast< sockaddr_un * >( alloca( ilen ) );
    inaddr->sun_family = AF_UNIX;
    std::copy( config.socket.begin(), config.socket.end(), inaddr->sun_path );
    inaddr->sun_path[ config.socket.size() ] = 0;
    bind( input, reinterpret_cast< const sockaddr * >( inaddr ), ilen ) == 0 || SYSDIE( "bind" );
    chmod( config.socket.c_str(), 0666 ) == 0 || SYSDIE( "chmod" );

    INFO( "Setting up socket for listening" );
    listen( input, config.max_workers ) == 0 || SYSDIE( "listen" );

    return input;
}

} // namespace exprtest

int main( int argc, char **argv )
{
    using namespace exprtest;
    // setup signals
    SignalFD sfd;
    sfd.add( { SIGUSR1, SIGTERM } );
    std::signal( SIGPIPE, SIG_IGN );

    char *cwd = get_current_dir_name();

    std::atomic< bool > end = false;
    std::atomic< bool > hotrestart = false;

    Config config;

    std::vector< std::string > args{ argv, argv + argc };
    std::vector< std::string > persistArgs;
    auto argit = args.begin();
    auto hasOpt = [&] { return argit != args.end(); };

    ++argit;

    int input = -1;
    std::string conffile;

    if ( hasOpt() && *argit == "--hotstart" ) {
        ++argit;
        ASSERT( hasOpt() );
        input = std::stoi( *argit );
        INFO( "hot start" );
        ++argit;
    }

    for ( auto *opt : { &conffile } ) {
        if ( hasOpt() ) {
            persistArgs.push_back( *argit );
            *opt = *argit;
            ++argit;
        }
    }

    if ( !conffile.empty() ) {
        ASSERT( brick::fs::access( conffile, R_OK ) );
        INFO( "loading config from " + conffile );
        config.load( conffile );
    } else {
        config.socket = "/var/lib/checker/socket";
        config.qdir_root = "/var/lib/checker/qdir";
        config.courses[ "ib015" ] = { "ib015", "hsExprTest", std::nullopt, false };
    }

    if ( input < 0 )
        input = start( config );


    INFO( "Listening on " + std::to_string( input ) );
    config.dump( std::cerr );

    setenv( "LANG", "en_US.UTF-8", 1 );

    std::array< struct pollfd, 2 > pollfds;
    std::memset( pollfds.begin(), 0, pollfds.size() );
    pollfds[0].fd = sfd.fd();
    pollfds[1].fd = input;
    pollfds[0].events = pollfds[1].events = POLLIN;

  main_loop:
    {
        WorkSet< FD > work( config.max_workers );
        WorkPool wp( config );
        Eval ev( config );

        wp.start( [ev, &work]( auto & ) mutable { ev.loop( work ); }, end );
        while ( !end ) {
            work.push( [input, &sfd, &pollfds, &hotrestart, &end]() -> std::optional< FD > {
                    INFO( "Accepting socket..." );
                    int p = poll( pollfds.data(), pollfds.size(), -1 );

                    if ( p < 0 ) { // error
                        SYSWARN( "poll failed" );
                        return { };
                    }
                    ASSERT_NEQ( p, 0 ); // timeout

                    if ( pollfds[0].revents & POLLIN ) {
                        switch ( int sig = sfd.readsig().signum() ) {
                            case SIGUSR1:
                                hotrestart = true;
                                [[fallthrough]];
                            case SIGTERM:
                                end = true;
                                return { };
                            default:
                                UNREACHABLE_F( "unexpected signal %d", sig );
                        }
                    }

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
                        return { };
                    }
                    return std::optional< FD >( std::move( isSock ) );
                } );
        }
        wp.wait();
    }

    if ( hotrestart ) {
        std::string hotstart = "--hotstart";
        auto sockstr = std::to_string( input );
        std::vector< char * > passargs { argv[ 0 ], hotstart.data(), sockstr.data() };
        for ( auto &s : persistArgs )
            passargs.push_back( s.data() );
        passargs.push_back( nullptr );
        INFO( "hot restart" );
        chdir( cwd ); // in case the working directory is deleted and re-created, this will go to it again
        execvp( argv[0], passargs.data() );

        WARN( "execvp failed, ignoring hot restart request" );
        end = false;
        hotrestart = false;
        goto main_loop;
    }
    INFO( "terminating" );
}
