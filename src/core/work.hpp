// (c) 2018 Vladimír Štill

#pragma once

#include "config.hpp"

#include <brick-assert>

#include <atomic>
#include <thread>
#include <memory>
#include <mutex>
#include <condition_variable>

namespace exprtest {

struct Worker
{
    ~Worker() { stop(); }

    template< typename Func >
    void start( Func f, std::atomic< bool > &stop ) {
        ASSERT( !_thr.joinable() );
        _stop = &stop;
        _thr = std::thread( [&stop, f]() mutable {
                while ( !stop.load( std::memory_order_relaxed ) )
                    f( stop );
            } );
    }

    void stop() {
        if ( _thr.joinable() ) {
            _stop->store( true, std::memory_order_relaxed );
            _thr.join();
        }
    }

    void wait() {
        if ( _thr.joinable() )
            _thr.join();
    }

    std::thread _thr;
    std::atomic< bool > *_stop;
};

struct WorkPool
{
    WorkPool( const Config &cfg ) : workers( cfg.max_workers ) { }
    ~WorkPool() { }

    template< typename Func >
    void start( Func f, std::atomic< bool > &stop ) {
        for ( auto &w : workers )
            w.start( f, stop );
    }

    void stop() {
        for ( auto &w : workers )
            w.stop();
    }

    void wait() {
        for ( auto &w : workers )
            w.wait();
    }

    std::vector< Worker > workers;
};

template< typename T >
class WorkSet
{
    using Guard = std::unique_lock< std::mutex >;
    struct Entry {
        std::optional< T > val;
        bool taken = false;
    };

  public:
    explicit WorkSet( int size ) : _storage( new Entry[ size ]() ), _size( size ) { }

    template< typename Lazy >
    bool push( Lazy l )
    {
        {
            Guard g( _mtx );
            _sigwrite.wait( g, [this] { return _set < _size; } );
        }
        // there is only one writer, so it is OK to break mutex here
        std::optional< T > val = l();
        {
            Guard g( _mtx );
            if ( !val ) {
                _terminate = true;
                _sigread.notify_all();
                return false;
            }

            ASSERT_LT( _set, _size );
            for ( auto &e : *this ) {
                if ( !e.val ) {
                    e.val = std::move( *val );
                    e.taken = false;
                    ++_set;
                    _sigread.notify_all();
                    return true;
                }
            }
            UNREACHABLE( "no empty slot found while count indicates there should be at least one" );
        }
    }

    struct Job {
        using opt = std::optional< T >;

        Job() = default;
        Job( WorkSet &ws, Entry &e ) : _ws( &ws ), _e( &e ) { }
        ~Job() {
            if ( _ws ) {
                Guard g( _ws->_mtx );
                _e->val = std::nullopt;
                --_ws->_set;
                --_ws->_taken;
                _ws->_sigwrite.notify_all();
            }
        }

        T &value() { ASSERT( _e ); return _e->val.value(); }
        operator bool() const { return _e; }

        T &operator*() { return value(); }
        T *operator->() { return &value(); }

      private:
        WorkSet *_ws = nullptr;
        Entry *_e = nullptr;
    };

    Job pop() {
        Guard g( _mtx );
        _sigread.wait( g, [this] { return _set - _taken > 0 || _terminate; } );
        if ( _terminate )
            return Job();
        for ( auto &e : *this ) {
            if ( e.val && !e.taken ) {
                e.taken = true;
                ++_taken;
                return Job( *this, e );
            }
        }
        UNREACHABLE( "no job found while count inticates there should be at least one" );
    }

  private:
    Entry *begin() { return _storage.get(); }
    Entry *end() { return _storage.get() + _size; }

    std::mutex _mtx;
    std::condition_variable _sigread, _sigwrite;
    std::unique_ptr< Entry[] > _storage;
    const int _size;
    int _set = 0, _taken = 0;
    bool _terminate = false;
};

}
