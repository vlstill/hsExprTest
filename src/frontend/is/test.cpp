#include <iostream>
#include <fcgio.h>
#include <cstdint>
#include <string>
#include <map>
#include <vector>
#include <mutex>
#include <optional>
#include <tuple>
#include <string_view>
#include <sstream>

using namespace std::literals;

namespace fcgi {

struct FCGIException : std::runtime_error {
    using std::runtime_error::runtime_error;
};

namespace detail {

std::once_flag initOnce;

std::string getRawPost( const FCGX_Request & request, std::istream &is, std::ostream &err) {
    char *lenStr = FCGX_GetParam( "CONTENT_LENGTH", request.envp );
    if ( !lenStr || lenStr[0] == '\0' )
        return "";
    try {
        auto len = std::min( std::stol( lenStr ), 2l * 1024 * 1024 );

        std::string buf;
        buf.resize( len );
        is.read( &buf[0], len );
        buf.resize( is.gcount() );

        // Chew up any remaining stdin - this shouldn't be necessary
        // but is because mod_fastcgi doesn't handle it correctly.

        // ignore() doesn't set the eof bit in some versions of glibc++
        // so use gcount() instead of eof()...
        do is.ignore( 1024 ); while ( is.gcount() == 1024 );

        return buf;
    } catch ( std::exception &ex ) {
        err << "error in post::getRaw: " + std::string( ex.what() );
        return "";
    }
}

}

void init() {
    std::call_once( detail::initOnce, [] {
            if ( int r = FCGX_Init(); r != 0 )
                throw FCGIException( "Init failed: " + std::to_string( r ) );
        } );
}

namespace url {

std::map< std::string, std::vector< std::string > > decode( std::string data )
{
    enum class CharState { Norm, Oct1, Oct2 };
    enum class ParamState { Key, Val };

    // automaton state
    CharState cst = CharState::Norm;
    ParamState pst = ParamState::Key;
    char acc[] = "00";

    // output creation
    std::map< std::string, std::vector< std::string > > out;
    std::string key, current;

    for ( auto c : data ) {
        if ( pst == ParamState::Key && c == '=' ) {
            key = std::move( current );
            current = "";
            pst = ParamState::Val;
        } else if ( pst == ParamState::Val && c == '&' ) {
            pst = ParamState::Key;
            out[ std::move( key ) ].emplace_back( std::move( current ) );
            key = "";
            current = "";
        } else {
            switch ( cst ) {
                case CharState::Norm:
                    if ( c == '+' )
                        current.push_back( ' ' );
                    else if ( c == '%' )
                        cst = CharState::Oct1;
                    else
                        current.push_back( c );
                    break;
                case CharState::Oct1:
                    acc[0] = c;
                    cst = CharState::Oct2;
                    break;
                case CharState::Oct2:
                    acc[1] = c;
                    current.push_back( std::strtol( acc, nullptr, 16 ) );
                    cst = CharState::Norm;
                    break;
            }
        }
    }
    if ( pst == ParamState::Val )
        out[ std::move( key ) ].emplace_back( std::move( current ) );
    return out;
}

} // namespace url

struct Request {

    struct ParamMap : std::map< std::string, std::vector< std::string > > {
      protected:
        using Base = std::map< std::string, std::vector< std::string > >;
        Base &base() { return *this; }
        friend Request;

      public:
        using std::map< std::string, std::vector< std::string > >::map;

        ParamMap( Base &&map ) : Base( map ) { }

        const std::vector< std::string > &operator[]( const std::string &key ) const {
            return this->at( key );
        }
    };

    Request() {
        init();
        FCGX_InitRequest( &_req, 0, 0 );
    }

    ~Request() {
        FCGX_Finish_r( &_req );
    }

    bool accepted() const { return _iob.has_value(); }

    bool accept() {
        auto r = FCGX_Accept_r( &_req );
        if ( r != 0 )
            return false;
        _iob.emplace( _req.in, _req.out, _req.err );
        _in.emplace( &std::get< 0 >( *_iob ) );
        _out.emplace( &std::get< 1 >( *_iob ) );
        _err.emplace( &std::get< 2 >( *_iob ) );

        _postMap.reset();
        _getMap.reset();
        _argsMap.reset();

        return true;
    }

    std::istream &in() { return _in.value(); }

    std::ostream &out() { return _out.value(); }
    std::ostream &err() { return _err.value(); }

    std::string param( const char *param ) {
        auto v = FCGX_GetParam( param, _req.envp );
        if ( !v )
            return "";
        return v;
    }

    const ParamMap &post() {
        if ( !_postMap && accepted() )
            _postMap.emplace( url::decode( detail::getRawPost( _req, in(), err() ) ) );
        return _postMap.value();
    }

    const ParamMap &get() {
        if ( !_getMap && accepted() )
            _getMap.emplace( url::decode( param( "QUERY_STRING" ) ) );
        return _getMap.value();
    }

    const ParamMap &args() {
        if ( !_argsMap && accepted() ) {
            _argsMap.emplace( post() );
            for ( auto &[ k, v ] : get() ) {
                std::copy( v.begin(), v.end(), std::back_inserter( _argsMap->base()[ k ] ) );
            }
        }
        return _argsMap.value();
    }

    std::optional< std::string_view > operator[]( const std::string &key ) {
        auto it = args().find( key );
        if ( it == args().end() || it->second.size() != 1 )
            return std::nullopt;
        return { it->second[0] };
    }

  private:
    FCGX_Request _req;
    std::optional< std::tuple< fcgi_streambuf, fcgi_streambuf, fcgi_streambuf > > _iob;
    // libstdc++ has some problems with tuple which make it impossible to create tuple of streams
    std::optional< std::istream > _in;
    std::optional< std::ostream > _out;
    std::optional< std::ostream > _err;
    std::optional< ParamMap > _postMap;
    std::optional< ParamMap > _getMap;
    std::optional< ParamMap > _argsMap;
};

} // namespace fcgi

int main() {

    fcgi::Request request;

    while ( request.accept() ) {

        request.out() << "Content-type: text/plain\r\n"
                      << "Access-Control-Allow-Origin: https://is.muni.cz\r\n"
                      << "Access-Control-Allow-Methods: POST\r\n"
                      << "\r\n";
        try {
            auto uco = request[ "uco" ].value_or( "[missing]" );
            auto ans = request[ "odp" ].value();
            auto id = request[ "id" ].value();
            std::string_view auth, course, qid;

            std::stringstream reply;

            auto sep1 = id.find( ':' );
            auto sep2 = id.find( ':', sep1 + 1 );
            if ( sep1 == std::string_view::npos && sep1 == sep2 ) {
                // legacy mode
                reply << "!evaluator is running in legacy mode, please update questionares!\n\n";
                course = "IB015";
                qid = id;
            }
            else if ( sep1 == std::string_view::npos || sep2 == std::string_view::npos ) {
                reply << "!invalid question id!\n\n";
                goto do_reply;
            } else {
                auth = id.substr( 0, sep1 );
                course = id.substr( sep1 + 1, sep2 - sep1 - 1 );
                qid = id.substr( sep2 + 1 );
            }

          do_reply:
            request.out() << "nok~~\n"
                          << reply.str()
                          << "uco = " << uco << "\n"
                          << "auth = " << auth << "\n"
                          << "course = " << course << "\n"
                          << "qid = " << qid << "\n"
                          << "odp: \n" << ans << "\n";
        } catch ( std::exception &ex ) {
            request.out() << "nok~~A fatal error occucred, please contact person responsible for these assignments\n";
            request.err() << "EXCEPTION: "s + ex.what();
        }
    }
}
