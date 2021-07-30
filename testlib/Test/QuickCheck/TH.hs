{-# LANGUAGE TemplateHaskell, UnicodeSyntax, ExplicitForAll, TupleSections,
             TypeFamilies, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.QuickCheck.TH
    ( deriveArbitrary
    , deriveGen
      -- * Convenience re-exports
    , def
    ) where

import Control.Arrow
import Control.Monad
import Data.CFG
import Data.Default.Class
import Data.Function.Memoize
import Data.List
import Data.Kind ( Constraint )
import GHC.Generics
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
    ( OccName (..), NameFlavour (..), NameSpace (..), PkgName (..), ModName (..) )
import Prelude.Unicode
import Test.QuickCheck

class CFGGenerator a where
    type ArbConstraint a :: Constraint
    type CoArbConstraint a :: Constraint

data GNonterm = GNonterm { typeName ∷ Name, vars ∷ [TyVarBndr ()] }
                deriving (Eq, Show)
data GTerm = GTArbitrary { arbType ∷ Type }
           | GTCtor { conName ∷ Name }
           deriving (Eq, Show)

-- put these into one splice and therefore one stage – this is necessary since
-- TyVarBndr and Type are mutually recursive and staging restrictions would
-- prevent them from seeing one another if defined in separate splices.
fmap concat . sequence $ deriveMemoizable <$>
    [ ''ModName
    , ''OccName
    , ''PkgName
    , ''NameSpace
    , ''NameFlavour
    , ''Name
    , ''TyLit
    , ''Specificity
    , ''TyVarBndr
    , ''Type
    -- the actual types we need, the previous are prerequisites
    , ''GNonterm
    , ''GTerm
    ]

-- | @'deriveArbitraryFor' ns ≡ 'deriveArbitrary' ns []@
deriveArbitraryFor ∷ [Name] → Q [Dec]
deriveArbitraryFor ns = deriveArbitrary ns []

-- | Derive arbitrary instances for (possibly) mutually recursive data types.
-- The first list of names gives types for which the 'Arbitrary' instances are
-- derived, while the second list gives additional types of data members of the
-- generated types which should also be generated by the same algorithm, but
-- which will not receive 'Arbitrary' instances. I.e. if you have a type @A@
-- that internally uses values of type @B@ (which can recurse back to @A@),
-- calling @deriveArbitrary [''A] [''B]@ will produce instance @'Arbitrary' A@
-- with a generator that also generates the @B@ values inside @A@ (the possible
-- instance @Arbitrary B@ will be ignored). This means that the size of
-- @B@-values is added to the size of the @A@-value that contains it, while
-- types not mentined in the @deriveArbitrary@ do not count to the size and use
-- the corresponding 'Arbitrary' instance instead.
--
-- Internaly, this generates a context-free grammar from all the types in the
-- first two arguments (with each types corresponging to a single nonterminal
-- symbol) and makes the arbitrary instances from random word generators
-- starting from nonterminals corresponding to types in the first argument.
-- Terminals of the grammar correspond to data constructors and to values of
-- types not mentioned in @deriveArbitrary@.
--
-- For parametrised types (i.e. type constructors), the generation is a bit more
-- involved – consider a following type: @data A a b = Base a Int | Rec a (A b a)@.
-- Please note that the type variables are swapped in the recursive appearance
-- in @Rec@. The grammar would look similar to the following:
--
-- @
-- <A a b> → "Base" "a" "Int" | "Rec" <A b a>
-- <A b a> → "Base" "b" "Int" | "Rec" <A a b>
-- @
--
-- The nonterminals (in @<…>@) represent data types of kind '*'. The terminals
-- (in @"…"@) represent either data constructors (@"Base"@, @"Rec"@), or data
-- types to be generated using the corresponding 'Arbitrary' Instances (@"a"@,
-- @"b"@, @"Int"@).
--
-- As we can see in this example, each combination of type variables gives rise
-- to a new "type" and therefore to a new nonterminal in the grammar.
deriveArbitrary ∷ [Name] → [Name] → Q [Dec]
deriveArbitrary ns = do
    gens ← sequence [(n, ) <$> deriveGen (n:rest)
                    | i ← [0 .. length ns - 1]
                    , let ([n], rest) = map snd *** map snd $ partition ((== i) . fst) $ zip [0..] ns
                    ]
    fmap concat . forM gens $ \(name, gen) → (⊥)
--        [mkArbitrary name gen, mkCoArbitrary name]

-- instance (Generic a, CFGGenerator a, ArbConstraint a) ⇒ Arbitrary a where
--     shrink = genericShrink

-- instance (CFGGenerator a, CoArbConstraint a) ⇒ CoArbitrary a where

deriveGen ∷ [Name] → Q Exp
deriveGen = (⊥)
