hsExprTest
==========

Automatic comparison of Haskell expressions and types based on QuickCheck.

Originally developed as bachelor thesis by Martin Jonáš on Faculty of
Informatics, Masaryk University in Brno, Czech Republic, the code was
later extended by Vladimír Štill and is going to be used as a testing tool
in the course IB015 Non-Imperative Programming on the same faculty.

It is currently in alpha state, so don't expect everything to be perfect.

[![Build Status](https://travis-ci.org/vlstill/hsExprTest.svg?branch=master)](https://travis-ci.org/vlstill/hsExprTest)

## Description

A tool for simple randomized testing of Haskell expressions based on the
QuickCheck library. The idea is that you should not need to know anything about
QuickCheck, testing should be automatized to maximal possible extend.

It is intended to test simple expressions, that is short code snippets
of few lines length at most, as an aid for beginning Haskell programmers.

### Main Features

-   data type equivalence checking
-   expression equality testing based on randomized testing using QuickCheck
-   counterexamples are printed if test fails, compilation error if code is
    malformed
-   support for testing of higher order functions including random function
    generation and their description in counterexamples (counterexamples are
    limited to random generated functions which are not higher order)
-   testing of polymorphic functions (automatic type degeneralization)
-   exceptions are threaded as possible output, that is if exceptions match
    on both expressions it is considered OK; this allows testing expressions
    containing for example integral division without having to care about
    divide by zero
-   support for custom test scripts

## Building and Installation

If you want to just to test it and don't want to get mess in your Haskell
packages you can use included Makefile which makes use of Cabal sandboxes
(requires Cabal >= 1.18). Just run `make env` and cabal will build everything
and put you in environment where hsExprTest binary is available in path
(and library can be also loaded in GHCI). Or run `make` and then use
`./hsExprTest`.

If you want to install it in your cabal profile just run `cabal install`
in root directory (works also for older cabals).

In any case you will need GHC at least 7.4.

If you get `GHC exception: cannot satisfy -package …` error, your Haskell
installation has (some) packages in nonstandard paths or your ghc-paths
package is wrongly build. Either use sandboxes using `make` or `make env`,
or set GHC_PACKAGE_PATH to proper package path (something like 
`/{GHC-INSTALL-ROOT}/lib/ghc-{GHC-VERSION}/package.conf.d`). Also the executable
depends on the library, so you should not make it by `ghc --make`.

## Running

Some examples (with output):

    $ hsExprTest compare-types --student="a -> b" --solution="b -> a"
    OK

    $ hsExprTest compare-expressions --student="f x y = mod x y" \
                                    --solution='f = mod' --expression-name="f"
    Success

    $ hsExprTest compare-expressions \
        --student="f x _ [] = x; f _ g xs = foldl1 g xs" \
        --solution='f x _ [] = x; f _ g xs = foldr1 g xs' --expression-name="f"
    DifferentValues: *** Failed! Falsifiable (after 9 tests and 27 shrinks):
    0
    {(-6,-2)->0, _->-6}
    [0,0,-2]
    -6 /= 0

    $ hsExprTest compare-expressions --student="f _ 0 = 0; f x y = mod x y" \
                                    --solution='f = mod' --expression-name="f"
    DifferentValues: *** Failed! Falsifiable (after 1 test):
    0
    0
    { EXCEPTION THROWN (ArithException): divide by zero } /= 0

    $ hsExprTest compare-expressions \
        --student="f _ 0 0 = 0; f g 0 1 = g 1 1; f g x y = g x y" \
        --solution='f _ 0 0 = 0; f g x y = x `g` y' --expression-name="f"
    DifferentValues: *** Failed! Falsifiable (after 603 tests and 24 shrinks):
    {(1,1)->0, _->1}
    0
    1
    1 /= 0

In any case counterexamples look like this: one input per line + differing
outputs on last line.

## Contributions & Bug Reporting

Contributions are welcome, please file pull requests.

Use GitHub bug reporting for bugs (I'm sure there are some).
