# Description of Haskell Testing Support

A tool for simple randomized testing of Haskell expressions based on the
QuickCheck library. The idea is that, unless you need to use custom data
structures, you should not need to know anything about QuickCheck, testing
should be automatized to maximal possible extend. It is intended to primarily
work with small code snippets.

## Main Features

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
    [`Testable.IO`](https://vlstill.github.io/hsExprTest/Test-Testable-IO.html)
    and
    [`Testable.IO.NoMonad`](https://vlstill.github.io/hsExprTest/Test-Testable-IO-NoMonad.html).
*   a service which connects the tester to a socket for use with testing
    frontend

## Haddock Documentation

Haddock documentation of the Haskell testing library is available at [github
pages](https://vlstill.github.io/hsExprTest/).

## Building and Installation

You will need GHC at least 8.2 (and cabal at least 1.22) and Python at least
\3.6 for the Haskell part and either GCC 7 or newer or clang 5 or newer for the
service. The easiest way to get a build is to checkout this repository and run
`make` in its top-level directory. This will create a `_build` directory with
the `hsExprTest-service` binary and it will install Haskell testing library to
your current cabal installation.

Alternatively, if you do not wish to install the service, or wish to install
`hsExprTest` into your profile using cabal, go to `src/hs` first and then run
`cabal install`.

After building, you should be able to run tests.

    $ ./src/hs/driver ASSINMET_FILE STUDENT_FILE

## Assignment Files

Assignment files describe an assignment and a way to test the solution, usually
by providing reference solution. Assignment files must contain header which
begins with comment with metadata. Metadata comments are like line comments, but
they have a space, `@` and space right after the comment beginning (e\.g. `-- @
` for Haskell). These comments are instructions for the language driver (Haskell
in the examples) on how to preprocess the student files.

### Type Comparison

**NOTE**: type comparison is temporarily unavailable

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

### Haskell Expression Comparison

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
expr = "binmap" -- instruct the QickCheck wrapper on what to check
timeout = 5 -- optionally modify the timeout

binmap :: (a -> a -> b) -> [a] -> [b]
binmap _ []       = []
binmap _ [_]      = []
binmap f (x:y:xs) = f x y : binmap f (y:xs)
```

The `expr = "<name>"` is needed to specify the name of expression to be
compared. After this test follows.  Available options can be found in [test
options](#test-options).

### Using source injection

Source injection is needed to specify helper functions, insert imports to
student files, or disallow imports. The source between `-- @ INJECT BEGIN` and
`-- @ INJECT END` is copied to the beginning of student's file, just after
module header.

The inject section can be anywhere in teacher's file, but it is recommended to
put it in the beginning as it is left in place in teacher's and put at the
beginning of student's file.  There can be only one inject section. Data types
should not be defined directly in inject section, as they would be distinct in
student's and teacher's module, see
[Importing data types](#importing-data-types) for more details.

#### Hiding functions from prelude

```haskell
import qualified Prelude
-- @ INJECT BEGIN
import Prelude hiding ( foldr, foldl, scanr, scanl, foldr1, foldl1, scanr1, scanl1 )
-- @ INJECT END

expr = "myfoldr"

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr = Prelude.foldr
```

Here student is tasked with programming `foldr`, therefore `foldr` needs to be
hidden from prelude. But solution file import of Prelude is not in inject
section, so `foldr` can be used in solution file.

#### Disallowing Imports for Student

Normally student is allowed to import any [Safe
Haskell](https://ghc.haskell.org/trac/ghc/wiki/SafeHaskell)  module in scope
(which includes `base`, `QuickCheck` and `hsExprTest` and their dependencies).
This can be disallowed by the annotation `-- @ allow imports: False`.

```haskell
-- @ allow imports: False
import qualified Data.Maybe

expr = "mapMaybe"

mapMaybe = Data.Maybe.mapMaybe
```

#### Importing Data Types

Data types have to be provided in extra module (which should be located in
question directory for the service, and local included (`-I`) directory for
direct running). If injected directly they would be distinct in student and
solution module.

question:
```haskell
-- @ INJECT BEGIN
import P20140704Data
-- @ INJECT END

expr = "mirrorTree"

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

### Using QuickCheck Modifiers & Patterns

List of QuickCheck modifiers can be found in [Test.QuickCheck.Modifiers][qcm].
Note that `Blind` modifier for inputs which are not instance of `Show` is
added automatically and functions are generated wrapped in the `Fun` type and
uncurried. There are also additional modifiers defined in `hsExprTest`, namely
[Test.QuickCheck.Range](https://vlstill.github.io/hsExprTest/Test-QuickCheck-Range.html),
[Test.QuickCheck.Literal](https://vlstill.github.io/hsExprTest/Test-QuickCheck-Literal.html), and
[Test.QuickCheck.Union](https://vlstill.github.io/hsExprTest/Test-QuickCheck-Union.html).

[qcm]: https://hackage.haskell.org/package/QuickCheck-2.8.1/docs/Test-QuickCheck-Modifiers.html

If QuickCheck (or hsExprTest's) modifiers should be used you should provide the
patterns into which arguments are bound by hand, with the suitable type
annotations.

```haskell
-- @ INJECT BEGIN
import Test.QuickCheck.Modifiers ( NonNegative ( NonNegative ) )
-- @ INJECT END

expr = "numbers"
pattern = [p| (NonNegative x, NonNegative y) |]

numbers :: Int -> Int -> Bool
/* ... */
```

The patterns are provided by the optional `pattern` global declaration in the
teacher's file. This variable should be set to a pattern using the
TemplateHaskell pattern notation `[p| … |]`{.haskell}. The pattern must define
all the variables used as the arguments of the tested expression (in order).
The simples case is binding all arguments in a tuple as seen in the example
above. You can optionally also specify types of the arguments: `[p|
(NonNegative x :: NonNegative Int, NonNegative y :: NonNegative Integer)
|]`{.hasell}. Furthermore, it is also possible to use more complex types in the
patterns:

```haskell
newtype Point = P (Integer, Integer, Integer) deriving ( Eq )
data Triangle = T Point Point Point deriving ( Eq )

instance Arbitrary Point where
    {- … -}

instance Arbitrary Triangle where
    {- … -}

pattern = [p| (T (P a) (P b) (P c)) |]
expr = "isTriangle"

isTriangle :: (Integer, Integer, Integer) -> (Integer, Integer, Integer) -> (Integer, Integer, Integer) -> Bool
isTriangle a b c = …
```

This will case hsExprTest to generate instances of `Triangle` and pass the
points to `isTriangle`.

In general, all the free, named variables in the pattern are matched
left-to-right to the arguments of the tested function (and possibly coerced by
a cast that can shed `newtype`s). The only exception are the "as" patterns:
`xss@(x:xs)`, in this case, only the internal variables (`x` and `xs`) are
matched. These patterns should be avoided.

#### Range modifier

To simplify commonly used pattern when parameters has to be from certain
range module `Test.QuickCheck.Range` (from `hsExprTest` package,
[Haddock](https://vlstill.github.io/hsExprTest/Test-QuickCheck-Range.html))
provides ranges with type-specified bounds (using GHC type literals
`-XTypeLits`).  See documentation of this module for more details, simple
example follows.

```haskell
-- @ INJECT BEGIN
import Test.QuickCheck.Range
-- @ INJECT END

expr = "clock"
pattern = [p| (Range hh :: Range Int 0 23, Range mm :: Range Int 0 59) |]

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

#### Arbitrary Instances

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


### Test Options for Haskell

There are two types of test options:

- Preprocessing options, which direct how the student's and teacher's file is
  processed before it is run.
    * these include the inject directives and the `-- @ exts:` directive which
     specifies GHC extensions which should be enabled in the student's file.
- Testing options which affect generation of the test expression by the Template
  Haksell testing library. These are normall Haskell variables in the teacher
  file.
    - `expr :: String` -- specifies the name of the test expession; this is the
      only mandatory option.
    - `pattern :: Q Pat` -- specifies [the pattern](#patterns) of the generated
      test inputs.
    - `typeOrder :: Test.Expr.Type.TypeOrder` -- specifies the allowed relation
      between the teacher's and student's type. Please note that you have to
      import the corresponding type and value constructor from the
      [`Test.Expr.Type` module](src/hs/testlib/Test/Expr/Type.hs).
    - `timeout :: Integer` -- a timeout in seconds.
    - `evaluator :: (…) -> IO ()` -- optionally, the teacher can provide an
      evaluator instead of their own solution, it his case, the QuickCheck
      comparison expression is not generated and the student's solution is
      passed as the only argument to the evaluator. The evaluator must end with
      non-zero exit code if and only if the student failed.
