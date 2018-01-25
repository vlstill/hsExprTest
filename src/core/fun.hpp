#pragma once

#include <utility>
#include <functional>

template< typename T, T ptr >
struct FunT {

    template< typename... As >
    auto operator()( As &&...args ) const
        noexcept( noexcept( ptr( std::forward< As >( args )... ) ) )
        -> decltype( ptr( std::forward< As >( args )... ) )
    {
        return ptr( std::forward< As >( args )... );
    }

    template< typename Cls >
    auto operator()( Cls &&cls ) const noexcept
        -> decltype( std::forward< Cls >( cls ).*ptr )
    {
        return std::forward< Cls >( cls ).*ptr;
    }

    template< typename Cls, typename... As >
    auto operator()( Cls &&cls, As &&...args ) const
        noexcept( noexcept( (std::forward< Cls >( cls ).*ptr)( std::forward< As >( args )... ) ) )
        -> decltype( (std::forward< Cls >( cls ).*ptr)( std::forward< As >( args )... ) )
    {
        return (std::forward< Cls >( cls ).*ptr)( std::forward< As >( args )... );
    }
};

#if __cplusplus > 201402L
template< auto ptr >
struct Fun : FunT< decltype( ptr ), ptr > { };
#endif

