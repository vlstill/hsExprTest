// (c) 2018 Vladimír Štill

#pragma once

#include <unistd.h>

namespace exprtest {

// unique_ptr-like holder for file descriptors
struct FD {
    FD() : fd( -1 ) { }
    explicit FD( int fd ) : fd( fd ) { }
    FD( const FD & ) = delete;
    FD( FD &&o ) : fd( o.fd ) { o.fd = -1; }

    explicit operator bool() { return fd >= 0; }
    explicit operator int() { ASSERT_LEQ( 0, fd ); return fd; }

    FD &operator=( int fd ) { this->fd = fd; return *this; }
    FD &operator=( const FD & ) = delete;
    FD &operator=( FD &&o ) {
        if ( fd >= 0 )
            close( fd );
        fd = o.fd;
        o.fd = -1;
        return *this;
    }

    ~FD() { if ( fd >= 0 ) close( fd ); }
    int fd;
};

}
