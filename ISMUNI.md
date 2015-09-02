# Writing questions for IS MUNI integration

This is documentation for creating questions, to see how to setup server,
see [backend][].

[backend]: https://github.com/vlstill/hsExprTest/blob/master/ISMUNI_backend.md

Questions are stored in question directory (defaults to `/var/lib/checker/qdir`),
they have extension `.q.hs`. Currently two types of questions are supported
**type comparing** and **expresion comparing** (using [QuickCheck](#quickcheck)).

Each question contains preamble, which has hsExprTestService comments
starting with `-- @` and test definition.

## Type comparison

```haskell
-- @ type
Bool  -> [[a]] -> Int
```

Type equality test contains preamble specifying it is type test and expected
type. Types are tested for equality, that is they can differ only in naming of
type variables, for example, allowed answer for this question would be
`Bool -> [[x]] -> Int` but neither `a -> [[b]] -> Int` nor `Bool -> [[Int]] -> Int`
would be allowed.

## Expression comparison

Expression comparing questions work in following way:

*   one function together with its type is compared in each test,
*   teacher's question file contains solution and name of function to be tested,
    (and optionally some matadate and code to be injected into student's
    solution -- see later),
*   student's solution is submitted using questioner in IS and sent to testing service
*   testing service performs preprocessing of student file (such as injection of
    code),
*   both student's and teacher's solutions are compiled,
*   types of student's and teacher's version of functions are compared (as in
    [type conparison](#type-comparison) case).
*   QuickCheck testing expression is built based on type of solution and executed
    (see [QuickCheck chapter](#quickcheck) for details),
*   test result is returned to IS.

Example of very basic question definition:

```haskell
-- @ expr: binmap
-- @ limit: 5000000

binmap :: (a -> a -> b) -> [a] -> [b]
binmap _ []       = []
binmap _ [_]      = []
binmap f (x:y:xs) = f x y : binmap f (y:xs)
```

In preamble, `-- @ expr: <name>` is needed to specify this is expression
comparison and the name of expression to be compared. After this test follows.
Other options are:

*   `limit`: time limit (in milliseconds) for test execution,
*   `inject`: To allow source injection (see later).

## Using source injection

Source injection is needed to specify helper functions, insert imports to
student files, or disallow imports. It is enabled by `-- @ inject` in preamble
and source between `-- @ INJECT BEGIN` and `-- @ INJECT END` is copied to the
beginning of student's file, just after module header.

### Hiding functions from prelude

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

### Disallowing imports for student

Normally student is allowed to import any module in scope (which includes
`base`, `QuickCheck` and `hsExprTest` and their dependencies). This can be
disallowed by injecting any function declaration into student file, after this
compilation of student file which attempts import will fail.

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

### Importing data types

Data types has to be provided in extra module (which should be located in
question directory), if injected directly they would be distinct
in student and solution module.

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

### Range modifier

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

## QuickCheck

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

### Arbitrary instances

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
