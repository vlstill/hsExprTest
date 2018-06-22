{-# LANGUAGE TemplateHaskell, LambdaCase, TupleSections #-}

-- | Property generation. Allows to generate property which compares two
-- implementations of the same functionality.
--
-- (c) 2018 Vladimír Štill

module Test.Expr.Property ( prop ) where

import Test.QuickCheck ( Blind (..), Arbitrary )
import Test.QuickCheck.Function ( Fun ( Fun ) )
import Control.Monad ( when, unless, replicateM, filterM, zipWithM )
import Language.Haskell.TH ( Q, Name, Cxt
                           , Info (..), Exp (..), Type (..), Pat (..), TyVarBndr (..)
                           , reportWarning, pprint, reify, newName, mkName )
import Data.Int ( Int16 )
import Data.List ( intercalate )

import Text.Printf.Mauke.TH ( sprintf )

import Test.Expr.Types
import Test.Expr.Utils

type TeacherFnName = Name
type StudentFnName = Name

-- | $(prop 'a 'b) :: Property
-- >>> quickCheck $(prop 'drop 'drop)
-- +++ OK, passed 100 tests.
--
-- >>> quickCheck $(prop 'drop 'take)
-- *** Failed! Falsifiable (after 3 tests):
-- 0
-- [()]
-- [()] /= []
prop :: Exp -> TeacherFnName -> StudentFnName -> Q Exp
prop comp teacher student = (,) <$> info teacher <*> info student >>= \case
    ((_, Just (tnam, ttype)), (_, Just (snam, stype))) -> testFun comp tnam ttype snam stype
    ((t, _), (s, _)) -> $(pfail "prop: Invarid arguments for prop:\n        %s\n        %s") (pprint t) (pprint s)

  where
    info x = ex <$> reify x

    ex :: Info -> (Info, Maybe (Name, Type))
    ex i@(VarI     nam typ _) = (i, Just (nam, typ))
    ex i@(ClassOpI nam typ _) = (i, Just (nam, typ))
    ex i@(DataConI nam typ _) = (i, Just (nam, typ))
    ex i                      = (i, Nothing)

testFun :: Exp -> TeacherFnName -> Type -> StudentFnName -> Type -> Q Exp
testFun comp tname ttype sname stype = do
    dtty <- degeneralize ttype
    dsty <- degeneralize stype

    when (dtty /= dsty) $ $(pfail "testFun: incompatible degeneralized types derived:\n        teacher: %s\n        student: %s")
                           (pprint dtty) (pprint dsty)

    let (targs, rty) = uncurryType dtty
    let ar = length targs
    retEq <- rty `hasInstance` ''Eq
    unless retEq . $(pfail "testFun: return type not comparable: %s") $ pprint rty

    xs <- replicateM ar (newName "x")

    pats <- zipWithM mkpat targs xs
    args <- zipWithM mkvar targs xs
    pure $ LamE pats (UInfixE (apply tname args `SigE` rty) comp (apply sname args `SigE` rty))

  where
    -- | construct a pattern from its type and variable name (@x@)
    -- * for function types, it constructs @Fun _ x@
    -- * if the type is not Show-able, wraps the pattern in 'Blind'
    -- * otherwise, it constructs @x@
    mkpat :: Type -> Name -> Q Pat
    mkpat t x = do
        arb <- hasArbitrary baseT
        sh <- hasShow baseT
        unless arb . $(pfail "testFun: no instance of arbitrary for %s") $ pprint t
        let typed = SigP base baseT
        if sh
        then pure typed
        else do
            reportWarning . $(sprintf "testFun: no instance of Show for %s, using Blind") $ pprint t
            pure $ ConP 'Blind [typed]
      where
        base | isFunctionType t = ConP 'Fun [WildP, VarP x]
             | otherwise        = VarP x

        baseT | isFunctionType t = ConT ''Fun `AppT` foldl AppT (TupleT arrt) ct `AppT` rt
              | otherwise        = t

        (ct, rt) = uncurryType t
        arrt = length ct

    mkvar :: Type -> Name -> Q Exp
    mkvar t x = pure base
      where
        base | isFunctionType t = VarE uc `AppE` VarE x
             | otherwise = VarE x

        (ta, _) = uncurryType t
        uc = mkName ("curry" ++ show (length ta))

type ClassName = Name
type TyVarName = Name

degeneralize :: Type -> Q Type
degeneralize t0 = degen [] [] $ normalizeContext t0
  where
    degen :: [TyVarBndr] -> Cxt -> Type -> Q Type
    degen bndr cxt (ForallT b c t) = degen (bndr ++ b) (cxt ++ c) t
    degen bndr0 cxt0 t = do
        substc <- extractCandidates bndr0
        cxt <- extractCxt cxt0
        sub <- filterSubstitutions substc cxt

        pure $ t // sub

    -- | extract simple contexts to
    extractCxt :: Cxt -> Q [(TyVarName, ClassName)]
    extractCxt = mapM ex
      where
        ex (AppT (ConT c) (VarT v)) = pure (v, c)
        ex x = $(pfail "degeneralize: Complex context %s not supported") $ pprint x

    extractCandidates :: [TyVarBndr] -> Q [(TyVarName, [Type])]
    extractCandidates = mapM ex
      where
        ex (PlainTV x) = ex (KindedTV x StarT)
        ex (KindedTV x StarT) = (x, ) <$> sequence
                                            [ [t| Integer |]  -- the default
                                            , [t| Rational |] -- for fractional
                                            , [t| Int16 |]    -- for bounded
                                            , [t| Double |]   -- for floating-point
                                            ]
        ex (KindedTV x (AppT (AppT ArrowT StarT) StarT)) = (x, ) . (:[]) <$> [t| [] |]
        ex ktv = $(pfail "degeneralize: Complex type variable %s not supported") $ pprint ktv

    filterSubstitutions :: [(TyVarName, [Type])] -> [(TyVarName, ClassName)] -> Q [(TyVarName, Type)]
    filterSubstitutions vs cxt = mapM (\(v, cs) -> subst v cs (map snd $ filter (\x -> fst x == v) cxt)) vs

    subst :: TyVarName -> [Type] -> [ClassName] -> Q (TyVarName, Type)
    subst v cands clss = filterM (\t -> and <$> mapM (\c -> t `hasInstance` c) clss) cands >>= \case
        []  -> $(pfail "degeneralize: Could not degeneralize %s with constraints %s")
                (pprint v) (intercalate "," $ map pprint clss)
        t:_ -> pure (v, t)

hasShow :: Type -> Q Bool
hasShow t = t `hasInstance` ''Show

hasArbitrary :: Type -> Q Bool
hasArbitrary t = t `hasInstance` ''Arbitrary