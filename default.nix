{ hsExprTestSrc, disable_shared ? false }:

let pkgs = import <nixpkgs> {};
    make = haskell : haskell.callPackage ./hsExprTest.nix {
        inherit hsExprTestSrc disable_shared;
    };
in rec {
    current = make pkgs.haskellPackages;
    ghc784 = make pkgs.haskell.packages.ghc784;
    ghc7101 = make pkgs.haskell.packages.ghc7101;
}
