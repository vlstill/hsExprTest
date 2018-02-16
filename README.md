ExprTest
==========

[![Build Status](https://travis-ci.org/vlstill/hsExprTest.svg?branch=master)](https://travis-ci.org/vlstill/hsExprTest)

Automatic testing of small programming assignments.

This project consists of two parts, an programming-language-agnostic driver for
testing which invokes teacher-provided script on student's solution and a tool
for comparison of Haskell expressions and types based on QuickCheck.

This tool is used in the [Non-Imperative
Programming](https://is.muni.cz/auth/predmet/fi/podzim2017/IB015)  course on
[Faculty of Informatics, Masaryk University (FI MU)](https://www.fi.muni.cz).
It can be used either locally (but that is not very useful) or connected to a
testing frontend, such as the frontend of [IS MU](https://is.muni.cz).
In either case, testing requires [assignment files](#assignment-files).

The work began with a bachelor thesis of Martin Jonáš on FI MU, the code was
later extended by Vladimír Štill and is used in the couse from autumn 2014.
As of autumn 2017, the new version 2 is planned to be used. Eventually, this
version should allow testing of programs in languages other than Haskell.


## Description

A tool for simple randomized testing of Haskell expressions based on the
QuickCheck library. The idea is that, unless you need to use custom data
structures, you should not need to know anything about QuickCheck, testing
should be automatized to maximal possible extend. It is intended to primarily
work with small code snippets.

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
-   support for custom comparators of output
*   a small library for mocking basic IO operations (with standard input and
    output): modules
    [`Testable.IO`](https://github.com/vlstill/hsExprTest/blob/master/src/hs/testlib/Testable/IO.hs)
    and
    [`Testable.IO.NoMonad`](https://github.com/vlstill/hsExprTest/blob/master/src/hs/testlib/Testable/IO/NoMonad.hs).
*   a service which connects the tester to a socket for use with testing
    frontend

## Building and Installation

You will need GHC at least 7.10 (and cabal at least 1.22) for the Haskell part
and either GCC 7 or newer or clang 4 or newer for the service. The easiest way
to get a build is to checkout this repository and run `make` in its top-level
directory. This will create a `_build` directory with cabal sandbox and two
binaries `hsExprTest` and `hsExprTest-service`.

Alternatively, if you do not wish to install the service, or wish to install
`hsExprTest` into your profile using cabal, go to `src/hs` first and then run
`cabal install`.

After building, you should be able to run tests.

    $ ./_build/hsExprTest ASSINMET_FILE STUDENT_FILE

If you get `GHC exception: cannot satisfy -package …` error, your Haskell
installation has (some) packages in nonstandard paths or your ghc-paths package
is wrongly build. Either use sandboxes using `make`, or set GHC_PACKAGE_PATH to
proper package path (something like
`/{GHC-INSTALL-ROOT}/lib/ghc-{GHC-VERSION}/package.conf.d`).

## Assignment Files

Assignment files describe an assignment and a way to test the solution, usually
by providing reference solution. There are now two modes of operation, Haskell
type comparison and Haskell expression comparison. Assignment files must contain
header which begins with comment with metadata. Metadata comments are like line
comments, but they have a space, `@` and space right after the comment beginning
(e\.g. `-- @ ` for Haskell).

### Type comparison

```haskell
-- @ type
Bool  -> [[a]] -> Int
```

Type equality test contains preamble specifying it is type test and expected
type. Types are tested for equality, that is they can differ only in naming of
type variables, for example, allowed answer for this question would be
`Bool -> [[x]] -> Int` but neither `a -> [[b]] -> Int` nor `Bool -> [[Int]] -> Int`
would be allowed.

Available comparison options can be found in [test options](#test-options).

### Expression comparison

Expression comparing questions work in following way:

*   one function together with its type is compared in each test,
*   teacher's question file contains solution and name of function to be tested,
    (and some matadata and optionally code to be injected into student's
    solution -- see later),
*   student's solution is submitted, either directly or using testing frontend
    and service
*   student solution is processed (code injection, …),
*   both student's and teacher's solutions are compiled,
*   types of student's and teacher's version of functions are compared (as in
    [type conparison](#type-comparison) case),
*   QuickCheck testing expression is built based on type of solution and executed
    (see [QuickCheck chapter](#quickcheck) for details),
*   test result is displayed or send using the service to frontend.

Example of very basic question definition:

```haskell
-- @ expr: binmap
-- @ limit: 5

binmap :: (a -> a -> b) -> [a] -> [b]
binmap _ []       = []
binmap _ [_]      = []
binmap f (x:y:xs) = f x y : binmap f (y:xs)
```

In preamble, `-- @ expr: <name>` is needed to specify this is expression
comparison and the name of expression to be compared. After this test follows.
Available options can be found in [test options](#test-options).

### Using source injection

Source injection is needed to specify helper functions, insert imports to
student files, or disallow imports. It is enabled by `-- @ inject` in preamble
and source between `-- @ INJECT BEGIN` and `-- @ INJECT END` is copied to the
beginning of student's file, just after module header.

The inject section can be anywhere in teacher's file, but it is recommended to
put it in the beginning as it is left in place in teacher's and put at the
beginning of student's file.  There can be only one inject section. Data types
should not be defined directly in inject section, as they would be distinct in
student's and teacher's module, see
[Importing data types](#importing-data-types) for more details.

#### Wrapping function

Sometimes it is required to modify input and/or output of assigned function
before testing it. In this case `INJECT` section and wrapper function can be used:

```haskell
-- @ expr: inInterval_wrap
-- @ limit: 2
-- @ inject

-- @ INJECT BEGIN
inInterval_wrap :: Int -> Int -> Int -> Bool
inInterval_wrap x a b = inInterval x (min a b) (max a b)
-- @ INJECT END

inInterval :: Int -> Int -> Int -> Bool
inInterval x a b = a <= x && x <= b
```

Note that: `expr` is set to wrapper function, which will be copied at the
beginning of student's solution. In this example, the task is to implement check
whether number is in interval, and it should be assumed that interval is valid
(`a <= b`), to ensure this, we wrap tested function in wrapper which swaps interval
bounds if necessary.

#### Hiding functions from prelude

```haskell
-- @ expr: myfoldr
-- @ limit: 2000000
-- @ inject

import qualified Prelude
-- @ INJECT BEGIN
import Prelude hiding ( foldr, foldl, scanr, scanl, foldr1, foldl1, scanr1, scanl1 )
-- @ INJECT END

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr = Prelude.foldr
```

Here student is tasked with programming `foldr`, therefore `foldr` needs to be
hidden from prelude. But solution file import of Prelude is not in inject
section, so `foldr` can be used in solution file.

#### Disallowing imports for student

Normally student is allowed to import any [Safe
Haskell](https://ghc.haskell.org/trac/ghc/wiki/SafeHaskell)  module in scope
(which includes `base`, `QuickCheck` and `hsExprTest` and their dependencies).
This can be disallowed by injecting any function declaration into student file,
after this compilation of student file which attempts import will fail.

```haskell
-- @ expr: mapMaybe
-- @ limit: 4000000
-- @ inject

import qualified Data.Maybe

-- @ INJECT BEGIN
no_imports_for_student = ()
-- @ INJECT END

mapMaybe = Data.Maybe.mapMaybe
```

#### Importing data types

Data types have to be provided in extra module (which should be located in
question directory for the service, and local included (`-I`) directory for
direct running). If injected directly they would be distinct in student and
solution module.

question:
```haskell
-- @ expr: mirrorTree
-- @ limit: 4000000
-- @ inject

-- @ INJECT BEGIN
import P20140704Data
-- @ INJECT END

mirrorTree :: BinTree a -> BinTree a
/* ... */
```

`P20140704Data.hs`:
```haskell
module P20140704Data where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Control.Monad
import Control.DeepSeq

data BinTree a = Empty
               | Node a (BinTree a) (BinTree a)
               deriving (Show, Eq)

instance Arbitrary a => Arbitrary (BinTree a) where
    arbitrary = sized arbitraryTree
    shrink Empty       = []
    shrink (Node v t1 t2) = [Empty, t1, t2]
                            ++ map (Node v t1) (shrink t2)
                            ++ map (flip (Node v) t2) (shrink t1)
                            ++ map (\w -> Node w t1 t2) (shrink v)

arbitraryTree :: Arbitrary a => Int -> Gen (BinTree a)
arbitraryTree 0 = return Empty
arbitraryTree n = frequency [ (1, return Empty)
                            , (4, liftM3 Node arbitrary (arbitraryTree (n `div` 2)) (arbitraryTree (n `div` 2)))
                            ]


instance NFData a => NFData (BinTree a) where
    rnf Empty = ()
    rnf (Node v t1 t2) = rnf v `seq` rnf t1 `seq` rnf t2 `seq` ()
```

### Using QuickCheck modifiers

List of QuickCheck modifiers can be found in [Test.QuickCheck.Modifiers][qcm].
Note that `Blind` modifier for inputs which are not instance of `Show` is
added automatically.

[qcm]: https://hackage.haskell.org/package/QuickCheck-2.8.1/docs/Test-QuickCheck-Modifiers.html

If QuickCheck (or hsExprTest's) modifiers should be used you have to wrap
tested expression in wrapper provided in inject section. Note that currently
this implies that types of wrappers are compared (as hsExprTest always
typechecks same expression as it tests -- this restriction will be lifter
in some future release).

```haskell
-- @ expr: wrap_numbers
-- @ limit: 2000000
-- @ inject

-- @ INJECT BEGIN
import Test.QuickCheck.Modifiers ( NonNegative ( NonNegative ) )

wrap_numbers :: NonNegative Int -> NonNegative Int -> ( Int, Int, Bool )
wrap_numbers (NonNegative x) (NonNegative y) = ( x, y, numbers x y )

-- @ INJECT END

numbers :: Int -> Int -> Bool
/* ... */
```

#### Range modifier

To simplify commonly used pattern when parameters has to be from certain
range module `Test.QuickCheck.Rang` (from `hsExprTest` package,
[Haddock](https://paradise.fi.muni.cz/~xstill/doc/hsExprTest/Test-QuickCheck-Range.html))
provides ranges with type-specified bounds (using GHC type literals
`-XTypeLits`).  See documentation of this module for more details, simple
example follows.

```haskell
-- @ expr: wrap_clock
-- @ limit: 2000000
-- @ inject

-- @ INJECT BEGIN
import Test.QuickCheck.Range

wrap_clock :: Range Int 0 23 -> Range Int 0 59 -> String
wrap_clock (Range hh) (Range mm) = clock hh mm
-- @ INJECT END

clock :: Int -> Int -> String
/* ... */
```

### QuickCheck

When comparing expressions `hsExprTest` automatically generates expression,
which compares results of invocation of student's solution to teacher's
solution and invokes [QuickCheck](https://hackage.haskell.org/package/QuickCheck)
to generate input for this expression.

This has several implications:

*   each data type which is parameter of tested expression has to be instance
    of `Arbitrary` typeclass
    *   when testing higher order functions this implies that parameters or
        functions in parameter must be instances of `CoArbitrary` and result
        must be `Arbitrary`,
    *   for custom data types you will have to
        [write arbitrary instance by hand](#arbitrary-instances),
*   furthermore result of whole expression must satisfy `NFData`, `Eq` and `Show`,
*   polymorphic types are automatically degeneralized (mostly to `Integer`),
    however, polymorphic type constructors of arity bigger then 0 are not
    supported (that is types like `a -> b` can be tested, but for example
    `t a -> t b` can not, as there is currently no way to determine what `t`
    should be),
*   should some of the solution throw an exception, this exception is caught
    in testing expression and is **not** propagated to QuickCheck
    *   this allows for comparing exceptional behavior,
    *   if both solutions throw exception for given input, those exceptions
        are comared (their type and exception message must match exactly),
    *   if just one of solutions throws exception, this is reported in
        test result.

This also means that there is no support for testing `IO` functions.

In case of test failure, QuickCheck will produce counterexample containing:

*   inputs of expression (each line for one parameter),
*   mismatched outputs (in form `<student's output> /= <teacher's output>`),
*   counterexample might contain `(*)` instead of input value if value is not
    instance of show, such types include
    *   anything which is not instance of `Show` except for most of function
        types, which [can be shown in QuickCheck][qce],
    *   higher order functions,
*   QuickCheck's shrinking will be used to minimize inputs for counterexample.

[qce]: https://hackage.haskell.org/package/QuickCheck-2.8.1/docs/Test-QuickCheck-Function.html

Currently QuickCheck performs 1000 tests, which should be enough for most
cases. However, **care should be taken that distribution of input values is
well suited for your testcase**, if not you should wrap tested function with
wrapper and use [QuickCheck](#using-quickcheck-modifiers) or
[Range](#range-modifier) modifiers, or write your own
[Arbitrary instance](#arbitrary-instances). Please note that inbuilt instances
for numeric data types usually generate only small values (somewhere in range
roughly -30 to 30).

#### Arbitrary instances

Each type which is input to testing expression must be instance of
[`Arbitrary`][arb] typeclass, see the link for more details. This typeclass
is used for automatic generation of random inputs and for input shrinking
(minimization of counterexample). Here is an example instance for tree-like
structure:

```haskell
data Filesystem = Folder [Filesystem]
                | File
                deriving ( Show )

instance Arbitrary Filesystem where
    arbitrary = sized arbitraryFilesystem
    shrink (Folder sub) = File : map Folder (shrink sub)
    shrink File         = []


arbitraryFilesystem :: Int -> Gen Filesystem
arbitraryFilesystem 0 = return File
arbitraryFilesystem n = frequency [ (3, choose (0, 5) >>= liftM Folder . arbitraryFSList n)
                                  , (1, return File)
                                  ]

arbitraryFSList :: Int -> Int -> Gen [Filesystem]
arbitraryFSList _ 0 = return []
arbitraryFSList n l = liftM2 (:) (arbitraryFilesystem (n `div` l)) (arbitraryFSList n (l - 1))
```

For recursive structures, `sized` combinator should be used to implement
arbitrary (note that the size parameter is decremented in `arbitraryFSList`).
`frequency` combinator chooses from one of the generators from the list, each
with probability proportional to first value in the tuple.

[arb]: https://hackage.haskell.org/package/QuickCheck-2.8.1/docs/Test-QuickCheck-Arbitrary.html


### Test options

Test options are written in special comments starting with `-- @ ` (the space
after `@` is required). These comments must be at the beginning of file, and
there must not be anything else between them (not even empty lines).

Options can be unparametrized (such as `-- @ type`, or parametrized, in
`-- @ key: value` format (space after `:` is required).

Available options are:

*   `type` -- this question is typechcekging question
*   `expr: NAME` -- this is expresion comparing question using `NAME` as expresion
    to compare
*   `limit` -- time limit (in seconds)[^limit] for test execution,
*   `inject` -- allow source injection (see
    [Using source injection](#using-source-injection)).
*   `typecheck: LIST-OF { = | < | > | u | n }` -- a space separated list of
    allowed type comparison results:
    *   `=` student's and teacher's type are equal (up to naming of type
        variables),
    *   `<` student's type is less general,
    *   `>` student's type is more general,
    *   `u` types are unifiable (also includes cases when they are equal,
        less/more general),
    *   `n` types are not unifiable (**this option is not available for expression
        comparing**).

    Default for `typecheck` is `=`, that is equality checking.

    Default `hintmode` is `typecheck`.

[^limit]: Due to backward compatibility with `hsExprTest` v1.* the limit larger
  than 1000 seconds is interpreted as number of milliseconds and converted
  automatically.

## IS MU Integration

If you wish to integrate with the IS MU, you should setup your `hsExprTest` and
its service on a server and make it accessible. See [backend][].

[backend]: https://github.com/vlstill/hsExprTest/blob/master/ISMUNI_backend.md

### Writing questions for IS MUNI integration

Questions are stored in question directory (defaults to `/var/lib/checker/qdir`),
they have extension `.q.hs`.


## Bug Reporting

Use GitHub bug reporting for bugs (I'm sure there are some).
