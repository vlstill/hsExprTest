use warnings;
use strict;
use v5.16;
use open qw(:encoding(UTF-8) :std);

say "{-# LANGUAGE TemplateHaskell, Unsafe #-}";
say "module StudentReflector ( studentAST ) where";
say "";

my $header = 1;
my $import = 0;
while ( <> ) {
    if ( /^(\s)*$/ || ($header && /^\s*--/) ) {
        print;
    }
    elsif ( $header && /^\s*{-.*-}/ ) {
        print;
    }
    elsif ( $header && /^\s*{-/ ) {
        print;
        while ( <> ) {
            print;
            last if /-}/
        }
    }
    elsif ( not $header ) {
        print "    $_";
    }
    elsif ( /^import/ ) {
        $import = 1;
        print;
    }
    elsif ( $import && /^\s/ ) {
        print;
    }
    else {
        $header = 0;
        $import = 0;
        say "";
        say "import Language.Haskell.TH ( Q, Dec )";
        say "";
        say "studentAST :: Q [Dec]";
        say "studentAST = [d|";
        print "    $_";
    }
}
say "  |]"
