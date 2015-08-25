{-# LANGUAGE TupleSections, NamedFieldPuns, PatternGuards #-}

-- (c) 2014, 2015 Vladimír Štill

module Types.Arguments
    ( getTestableType
    , getDegeneralizedTypes
    , degeneralize
    , buildTestExpression
    ) where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Arrow
import Data.Bool
import Data.Set ( Set, toList, fromList )
import Data.Monoid
import Types
import Types.Parser ( parseType )
import Language.Haskell.Interpreter ( Interpreter, typeChecks, typeOf )

-- | Note that polymorphic type (both uncostrained and constrained) can belong
-- to any typeclass, and therefore isTypeclass "a" "AnyClassInScope" returns
-- always true.
isTypeclass :: Type -> String -> Interpreter Bool
isTypeclass ty typeclass = do
    tc <- typeChecks expr
    if not tc
        then return False
        else either (const False) plainType . parseType <$> typeOf expr
  where
    expr = concat [ "[ undefined :: ", formatType ty, ","
                  , " undefined :: ", typeclass, " a => a ]"
                  ]

isTypeclasses :: Type -> [String] -> Interpreter Bool
isTypeclasses ty = fmap and . mapM (ty `isTypeclass`)

getDegeneralizedTypes :: TypeExpression -> ExceptT String Interpreter [Type]
getDegeneralizedTypes = fmap degeneralize . getTestableType

getTestableType :: TypeExpression -> ExceptT String Interpreter TypeExpression
getTestableType (TypeExpression (TypeContext ctx) ty) = finalize <$> gtt False ty
  where
    finalize :: Set (TypeClass, [Type]) -> TypeExpression
    finalize ctxnew = TypeExpression (TypeContext . toList $ ctxnew `mappend` fromList ctx) ty
    
    gtt :: Bool -> Type -> ExceptT String Interpreter (Set (TypeClass, [Type]))
    gtt nested = foldArgumentsM funapp (arg nested)
    funapp a b = return $ a `mappend` b
    arg nested isret typ
        | Just _ <- splitFunApp typ      = gtt True typ
        | Just ts <- unwrapTupleType typ = mconcat <$> mapM (gtt True) ts
        | Just t <- unwrapListType typ   = gtt True t
        | TypeConstructor _ <- typ       = checkTestable
        | TypeVariable var <- typ        = return $ mkTestable var
        | Just (TyCon _, args) <- splitConApp typ
                                         = (mconcat <$> mapM (gtt True) args) >>= \x -> checkTestable >> return x
        | otherwise                      = throwE $ "Not testable, don't know how to test `" ++ formatType typ ++ "'."
      where
        checkTestable = lift (isTypeclasses typ $
                                ifL ["Eq"] (not nested && isret)
                                ++ bool ["Arbitrary"] ["CoArbitrary", "Function"] (nested && not isret))
                            >>= flip when (throwE ("Not testable: `" ++ formatType typ ++ "'.")) . not
                            >> return mempty
        mkTestable var = fromList . map (, [TypeVariable var]) $
              "Show" : bool ["Arbitrary"] [ "CoArbitrary", "Function" ] (nested && not isret)
                     ++ ifL ["Eq"] (not nested && isret)

        ifL l True  = l
        ifL _ False = []

buildTestExpression :: String -> String -> Type -> Interpreter String
buildTestExpression st so ty = do
    (binds, pars) <- ($ ty) $ functionTypes >>> fst >>> zip [0..] >>> mapM (first show >>> uncurry arg) >>> fmap unzip
    return . unwords $ if null binds
        then [ "AnyProperty (", withWitness st, "<==>", withWitness so, ")" ]
        else "AnyProperty (\\" : binds ++ [ "->", withWitness st ] ++
                   pars ++ [ "<==>", withWitness so ] ++ pars ++ [ ")" ]
  where
    withWitness fn = "((" ++ fn ++ ") `withTypeOf` (undefined :: " ++ formatType ty ++ "))"
    arg :: String -> Type -> Interpreter (String, String)
    arg i typ
        | isHigherOrderFunction typ = return (bindBF, parF)
        | isFunction typ            = return (bindF, parF)
        | otherwise = bool ("(Blind " ++ xi ++ ")", xi) (xi, xi) <$> typ `isTypeclass` "Show"
      where
        docurry = (> 1) . length . fst $ functionTypes typ
        bindF = "(Fun _ f" ++ i ++ ")"
        bindBF = "(Blind f" ++ i ++ ")"
        parF = bool ("f" ++ i) ("(gcurry (f" ++ i ++ " :: " ++ formatType tftype ++ "))") docurry
        tftype = uncurry (-->) . first tupleType . functionTypes $ typ
        xi = "x" ++ i

-- | This function generates monomorphic instances out of possibly polymorphic
-- arguments
--
-- TODO: Actually select proper types and possibly generate more instances,
-- not just degeneralize every type variable to Integer
degeneralize :: TypeExpression -> [ Type ]
degeneralize ex@(TypeExpression _ ty) = [ ty // degenSubst ]
  where
    degenSubst = map (\x -> (x, TypeConstructor (TyCon "Integer"))) vars
    vars = typeVars ex
