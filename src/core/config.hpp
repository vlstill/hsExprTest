// (c) 2018 Vladimír Štill

#pragma once

#include <map>
#include <string>
#include <fstream>
#include <string_view>
#include <optional>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused"
#pragma GCC diagnostic ignored "-Wunused-function"
#pragma GCC diagnostic ignored "-Wignored-attributes"
#include <brick-assert>
#include <brick-fs>
#include <brick-string>
#pragma GCC diagnostic pop

namespace exprtest {

struct Config
{
    struct Course
    {
        std::string name;
        std::string checker;
        std::optional< std::string > qdir;
        bool isolation = false;
        bool hint = false;
    };

    Config() = default;

    explicit Config( const std::string &conffile ) {
        load( conffile );
    }

    // a very simple YAML-like config loader
    void load( const std::string &conffile ) {
        std::ifstream in( conffile );
        std::string buf;
        bool cs;
        std::vector< Course > vcourses;

        while ( std::getline( in, buf, ':' ) ) {
            std::string key = buf;
            std::getline( in, buf );
            std::string val = buf;

            if ( key == "socket" ) {
                socket = strip( val );
                cs = false;
            } else if ( key == "qdir_root" ) {
                qdir_root = strip( val );
                cs = false;
            } else if ( key == "max_workers" ) {
                std::string sval( strip( val ) );
                max_workers = std::stoi( sval );
                cs = false;
            } else if ( key == "courses" ) {
                cs = true;
            } else if ( cs ) {
                ASSERT( indented( key ) );
                if ( new_arr_entry( key ) )
                    vcourses.emplace_back();
                auto skey = strip_key( key );
                if ( skey == "name" )
                    vcourses.back().name = strip( val );
                else if ( skey == "checker" )
                    vcourses.back().checker = strip( val );
                else if ( skey == "qdir" )
                    vcourses.back().qdir = strip( val );
                else if ( skey == "isolation" )
                    vcourses.back().isolation = to_bool( val );
                else if ( skey == "hint" )
                    vcourses.back().hint = to_bool( val );
            } else
                UNREACHABLE_F( "unexpected config key: %s", key.c_str() );
        }

        for ( auto c : vcourses ) {
            ASSERT( !c.name.empty() && "courses cannot have empty/unspecified names" );
            auto r = courses.emplace( c.name, std::move( c ) ).second;
            ASSERT( r && "duplicated course" );
        }
    }

    static std::string_view strip( std::string_view s ) {
        while ( !s.empty() && std::isspace( s[0] ) )
            s.remove_prefix( 1 );
        while ( !s.empty() && std::isspace( s.end()[-1] ))
            s.remove_prefix( 1 );
        return s;
    }

    static std::string_view strip_key( std::string_view s ) {
        s = strip( s );
        if ( !s.empty() && s[0] == '-' )
            s.remove_prefix( 1 );
        return strip( s );
    }

    static bool indented( std::string_view s ) {
        return !s.empty() && std::isspace( s[0] );
    }

    static bool new_arr_entry( std::string_view s ) {
        s = strip( s );
        return !s.empty() && s[0] == '-';
    }

    static bool to_bool( std::string_view val ) {
        auto sval = strip( val );
        if ( sval == "true" || sval == "yes" || sval == "1" )
            return true;
        if ( sval == "false" || sval == "no" || sval == "0" )
            return false;
        UNREACHABLE_F( "unexpected value for isolation: %s", val.data() );
    }

    const Course &operator[]( const std::string &c ) const {
        return courses.at( c );
    }

    std::string qdir( const std::string &c ) const {
        if ( auto q = (*this)[c].qdir )
            return brick::fs::isAbsolute( *q ) ? *q : brick::fs::joinPath( qdir_root, *q );
        return brick::fs::joinPath( qdir_root, c );
    }

    void dump( std::ostream &os ) {
        os << "config:\n    socket: " << socket
           << "\n    max_workers: " << max_workers
           << "\n    courses:";
        for ( auto [ course, info ] : courses ) {
            os << "\n      - name: " << course
               << "\n        checker: " << info.checker
               << "\n        qdir: " << qdir( course )
               << "\n        isolation: " << info.isolation
               << "\n        hint: " << info.hint;
        }
        os << std::endl;
    }

    int max_workers = 4;
    std::string socket;
    std::string qdir_root;
    std::map< std::string, Course > courses;
};

}
