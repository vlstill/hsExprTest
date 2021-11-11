{-# LANGUAGE TemplateHaskell, Unsafe, NamedFieldPuns, BangPatterns, DataKinds,
             KindSignatures, GADTs, FlexibleInstances, StandaloneDeriving,
             TypeOperators, TupleSections, MultiParamTypeClasses #-}
module TestSpec (
    -- * Test Specification
    TestSpec ( TestSpec, group, name, property, pointWeight, info, maxSucc,
               maxShrinks, caseTimeoutMicroSec, testTimeoutSec ),
    makeTests,
    RunCondMode ( Spec, Exec, Dep ),
    RunCond ( Fn, Or, And, Always, With, Depends ), (∧), (∨),
    -- * TestSpec modifiers
    defaults, tutorMarked,
    -- * Testing utilities
    student's
    ) where

import Control.Monad ( when )
import Control.Monad.Trans.Maybe
import Data.Maybe ( isNothing, isJust )
import Data.String ( IsString, fromString )
import Data.List ( intercalate )
import Data.Char ( toUpper, toLower )
import Data.Type.Equality
import Data.Type.Bool

import Test.QuickCheck ( Property )

import Language.Haskell.TH ( Q, Name, Exp (..), Lit (..), lookupValueName,
                             reportWarning )

data RunCondMode = Spec | Exec | Dep

data RunCond (mode :: RunCondMode) test dep where
    -- | 'Fn' cannot be used in dependencies, use 'Always', or just plain strings with OverloadedStrings in spec
    Fn      :: (mode == 'Dep) ~ 'False => test -> RunCond mode test dep
    Or      :: [RunCond mode test dep] -> RunCond mode test dep
    And     :: [RunCond mode test dep] -> RunCond mode test dep
    -- | Always can be used in specification and dependencies
    Always  :: (mode == 'Spec || mode == 'Dep) ~ 'True => test -> RunCond mode test dep
    With    :: RunCond 'Spec test dep -> String -> RunCond 'Spec test dep
    -- | dependencies cannot be nested
    Depends :: (mode == 'Dep) ~ 'False => RunCond mode test dep -> RunCond 'Dep dep dep -> RunCond mode test dep

type TestSpecCond = RunCond 'Spec String String

deriving instance (Show t, Show d) => Show (RunCond m t d)

(∧) :: RunCond m t d -> RunCond m t d -> RunCond m t d
And as ∧ And bs = And (as ++ bs)
And as ∧ b      = And (as ++ [b])
a ∧ And bs      = And (a : bs)
a ∧ b           = And [a, b]

(∨) :: RunCond m t d -> RunCond m t d -> RunCond m t d
Or as ∨ Or bs = Or (as ++ bs)
Or as ∨ b     = Or (as ++ [b])
a ∨ Or bs     = Or (a : bs)
a ∨ b         = Or [a, b]

infixr 7 ∧
infixr 6 ∨
infix 5 `With`
infix 4 `Depends`

instance IsString t => IsString (RunCond 'Spec t d) where
    fromString = Fn . fromString

instance IsString d => IsString (RunCond 'Dep d d) where
    fromString = Always . fromString

embrace :: String -> String
embrace xs = "(" ++ xs ++ ")"

unspace :: String -> String
unspace = intercalate "\xa0" . words  -- encode spaces as non-breakable for the sake of bash

condPrettyName :: RunCond mode String dep -> String
condPrettyName (Fn x)        = unspace x
condPrettyName (Or xs)       = embrace $ intercalate "\xa0∨\xa0" (condPrettyName <$> xs)
condPrettyName (And xs)      = embrace $ intercalate "\xa0∧\xa0" (condPrettyName <$> xs)
condPrettyName (Always note) = unspace note
condPrettyName (With _ name) = unspace name
condPrettyName (Depends c _) = condPrettyName c

checkImplemented :: RunCond mode String String -> Q Bool
checkImplemented (Fn name)     = isJust <$> lookupValueName ("Student." ++ name)
checkImplemented (Always _)    = pure True
checkImplemented (And xs)      = and <$> mapM checkImplemented xs
checkImplemented (Or xs)       = or <$> mapM checkImplemented xs
checkImplemented (With cond _) = checkImplemented cond
checkImplemented (Depends t _) = checkImplemented t

extractPropertyNames :: RunCond 'Spec String String -> MaybeT Q (RunCond 'Exec (String, Name) String)
extractPropertyNames = go
  where
    look :: String -> MaybeT Q Name
    look name = MaybeT . lookupValueName $ "prop_" <> name

    go :: RunCond 'Spec String String -> MaybeT Q (RunCond 'Exec (String, Name) String)
    go (Fn x) = Fn . (x,) <$> look x
    go (Always x) = lookByPrettyName x
    go (Or xs) = Or <$> mapM go xs
    go (And xs) = And <$> mapM go xs
    go (With _ x) = lookByPrettyName x
    go (Depends t d) = Depends <$> go t <*> pure (unspaceDep d)

    unspaceDep :: RunCond 'Dep String String -> RunCond 'Dep String String
    unspaceDep (Always x) = Always (unspace x)
    unspaceDep (Or os)    = Or  (map unspaceDep os)
    unspaceDep (And as)   = And (map unspaceDep as)

    lookByPrettyName x = Fn . (unspace x,) <$> look (onHead toLower . concatMap (onHead toUpper) $ words x)

    onHead :: (a -> a) -> [a] -> [a]
    onHead f (x:xs) = f x : xs
    onHead _ [] = []

-- | A nasty hack to make this compatible with both older GHC/TH which does not
-- allow for tuple sections in TupE and now ones which do (without using CPP).
-- https://hackage.haskell.org/package/template-haskell-2.16.0.0/docs/Language-Haskell-TH-Syntax.html#t:Exp
-- https://hackage.haskell.org/package/template-haskell-2.15.0.0/docs/Language-Haskell-TH-Syntax.html#t:Exp
class PossiblyWrapMaybe a b where possiblyWrapMaybe :: a -> b
instance PossiblyWrapMaybe a a where possiblyWrapMaybe = id
instance PossiblyWrapMaybe a (Maybe a) where possiblyWrapMaybe = Just

liftCond :: Maybe (RunCond 'Exec (String, Name) String) -> Exp -- (Maybe (RunCond Exec (String, Property) String))
liftCond = maybe nothingE (justVarE . go)
  where
    nothingE = ConE 'Nothing
    justVarE = AppE (ConE 'Just)

    go :: RunCond 'Exec (String, Name) String -> Exp
    go (Fn (name, prop)) = ConE 'Fn `AppE` TupE (map possiblyWrapMaybe [stringE name, VarE prop])
    go (Or xs)           = ConE 'Or `AppE` ListE (map go xs)
    go (And xs)          = ConE 'And `AppE` ListE (map go xs)
    go (Depends t d)     = ConE 'Depends `AppE` go t `AppE` goDep d

    goDep :: RunCond 'Dep String String -> Exp
    goDep (Or xs)       = ConE 'Or `AppE` ListE (map goDep xs)
    goDep (And xs)      = ConE 'And `AppE` ListE (map goDep xs)
    goDep (Always name) = ConE 'Always `AppE` stringE name

    stringE = LitE . StringL

data TestSpec = TestSpec { group :: String,
                           name :: String,
                           property :: Maybe (RunCond 'Exec (String, Property) String),
                           pointWeight :: Int,
                           info :: Maybe String,
                           maxSucc :: Maybe Int,
                           maxShrinks :: Maybe Int,
                           caseTimeoutMicroSec :: Int,
                           testTimeoutSec :: Int
                         }

makeTests :: [(TestSpecCond, Int, Q Exp)] -> Q Exp
makeTests = fmap ListE . mapM makeTest

makeTest :: (TestSpecCond, Int, Q Exp) -> Q Exp
makeTest (runCond, weight, modifier) = do
                pNames <- runMaybeT $ extractPropertyNames runCond
                let name = condPrettyName runCond
                fn <- boolToMaybe <$> checkImplemented runCond
                let props = liftCond (fn *> pNames)
                when (isNothing pNames) . reportWarning $ "Could not auto-find property for " ++ name
                [| ($(modifier)) $ TestSpec { group = "verity",
                                    name,
                                    pointWeight = weight,
                                    info = Nothing,
                                    maxSucc = Nothing,
                                    maxShrinks = Nothing,
                                    caseTimeoutMicroSec = 1000000, -- 1s
                                    testTimeoutSec = 20,
                                    property = $(pure props) } |]
  where
    boolToMaybe :: Bool -> Maybe ()
    boolToMaybe False = Nothing
    boolToMaybe True  = Just ()

defaults, tutorMarked :: Q Exp
defaults = [| id |]
tutorMarked = [| \s -> s { info = Just "tutor marked" } |]

student's :: String -> Q Exp
student's f = lookupValueName ("Student." ++ f) >>= maybe [|undefined|] (pure . VarE)
