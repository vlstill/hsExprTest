// (c) 2018 Vladimír Štill

#pragma once

#include <sys/signalfd.h>
#include <signal.h>
#include <brick-assert>
#include <initializer_list>

#include "fd.hpp"

namespace exprtest {

struct SignalFD
{
    SignalFD() : _sigfd( -1 ) {
        int r = sigemptyset( &_sigset );
        ASSERT( r == 0 );
    }
    ~SignalFD() {
        sigprocmask( SIG_UNBLOCK, &_sigset, nullptr );
    }

    void add( std::initializer_list< int > sigs ) {
        add( sigs.begin(), sigs.end() );
    }

    void add( const std::vector< int > &sigs ) {
        add( sigs.begin(), sigs.end() );
    }

    void add( int sig ) {
        add( { sig } );
    }

    struct Siginfo : signalfd_siginfo
    {
        void clear() { std::memset( this, 0, sizeof( signalfd_siginfo ) ); }

        int signum() const { return ssi_signo; }
    };

    static_assert( sizeof( Siginfo ) == sizeof( signalfd_siginfo ) );

    Siginfo readsig() {
        Siginfo si;
        int r = read( _sigfd.fd, &si, sizeof( Siginfo ) );
        if ( r != sizeof( Siginfo ) )
            si.clear();
        return si;
    }

    template< typename ItB, typename ItE >
    void add( ItB begin, ItE end ) {
        for ( ; begin != end; ++begin ) {
            int r = sigaddset( &_sigset, *begin );
            ASSERT( r == 0 );
        }
        _sigfd.fd = signalfd( _sigfd.fd, &_sigset, SFD_CLOEXEC );
        ASSERT( !!_sigfd );
        // make sure signal are not delivered normally
        sigprocmask( SIG_BLOCK, &_sigset, nullptr );
    }

    int fd() { return _sigfd.fd; }

  private:
    FD _sigfd;
    sigset_t _sigset;
};

}
