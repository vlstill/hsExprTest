# hsExprTestService and IS MUNI integration

## Setting up server

### What is needed

*   build of hsExprTestService
*   question directory
*   writable directory for service socket

### TODO

## Writing questions

Questions are stored in question directory (defaults to /var/lib/checker/qdir),
they have extension '.q.hs'.

Each question contains preamble, which has hsExprTestService comments
starting with `-- @` and test definition.

### Type comparison

    -- @ type
    Bool  -> [[a]] -> Int

Type equality test contains preamble specifying it is type test and expected
type.

### Basic question

    -- @ expr: binmap
    -- @ limit: 5000000

    binmap :: (a -> a -> b) -> [a] -> [b]
    binmap _ []       = []
    binmap _ [_]      = []
    binmap f (x:y:xs) = f x y : binmap f (y:xs)

Here we can see simple expression comparison, in preamble, `-- @ expr: <name>`
is needed to specify this is expression comparison and the name of expression
to be compared. After this test follows. Other options are:

*   `limit`: time limit (in milliseconds) for test execution,
*   `inject`: To allow source injection (see later).

### Using source injection

Source injection is needed to specify helper functions, insert imports to
student files, or disallow imports. It is enabled by `-- @ inject` in preamble
and source between `-- @ INJECT BEGIN` and `-- @ INJECT END` is copied to the
beginning of student's file, just after module header.

#### Hiding functions from prelude

    -- @ expr: myfoldr
    -- @ limit: 2000000
    -- @ inject

    import qualified Prelude
    -- @ INJECT BEGIN
    import Prelude hiding ( foldr, foldl, scanr, scanl, foldr1, foldl1, scanr1, scanl1 )
    -- @ INJECT END

    myfoldr :: (a -> b -> b) -> b -> [a] -> b
    myfoldr = Prelude.foldr

Here student is tasked with programming `foldr`, therefore `foldr` needs to be
hidden from prelude. But solution file import of Prelude is not in inject 
section, so `foldr` can be used in solution file.

#### Disallowing imports for student

    -- @ expr: mapMaybe
    -- @ limit: 4000000
    -- @ inject

    import qualified Data.Maybe

    -- @ INJECT BEGIN
    no_imports_for_student = ()
    -- @ INJECT END

    mapMaybe = Data.Maybe.mapMaybe

Normally student is allowed to import any module in scope (which includes
`base`, `QuickCheck` and `hsExprTest` and their dependencies). This can be 
disallowed by injecting any function declaration into student file, after this
compilation of student file which attempts import will fail.

#### Importing data types

    -- @ expr: mirrorTree
    -- @ limit: 4000000
    -- @ inject

    -- @ INJECT BEGIN
    import P20140704Data
    -- @ INJECT END

    mirrorTree :: BinTree a -> BinTree a
    mirrorTree Empty = Empty
    mirrorTree (Node v t1 t2) = Node v (mirrorTree t2) (mirrorTree t1)

Data types has to be provided in extra module (which should be located in
question directory), if injected directly they would be distinct
in student and solution module.

#### Using QuickCheck modifiers

    -- @ expr: wrap_numbers
    -- @ limit: 2000000
    -- @ inject

    -- @ INJECT BEGIN
    import Test.QuickCheck.Modifiers ( NonNegative ( NonNegative ) )

    wrap_numbers :: NonNegative Int -> NonNegative Int -> ( Int, Int, Bool )
    wrap_numbers (NonNegative x) (NonNegative y) = ( x, y, numbers x y )

    -- @ INJECT END

    numbers :: Int -> Int -> Bool
    numbers x y = digits x == digits y
      where digits 0 = 0
            digits x = x `mod` 10 + digits (x `div` 10)

If QuickCheck (or hsExprTest's) modifiers should be used you have to wrap
tested expression in wrapper provided in inject section. Note that currently
this implies that types of wrappers are compared (as hsExprTest always 
typechecks same expression as it tests -- this restriction will be lifter
in some future release).

### QuickCheck requirements

Each data type which is parameter of tested expression has to be instance
of `Arbitrary` typeclass, when testing higher order functions this implies
that parameters or functions in parameter must be instances of `CoArbitrary`
and result must be `Arbitrary`. Furthermore result of whole expression must
satisfy `NFData` and `Eq`.

Polymorphic types are automatically degeneralized (mostly to `Integer`),
however, polymorphic type constructors of arity bigger then 0 are not
supported. That is types like @a -> b@ can be tested, but for example 
@t a -> t b@ can not, as there is currently no way to determine what @t@
should be.

### Range modifier

To simplify commonly used pattern when parameters has to be from certain
range module `Test.QuickCheck.Rang` (from `hsExprTest` package) provides
ranges with type-specified bounds (using GHC type literals `-XTypeLits`).
See documentation of this module for more details, simple example follows.

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


