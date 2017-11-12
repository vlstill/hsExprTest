{-# LANGUAGE TupleSections, ScopedTypeVariables, LambdaCase #-}

-- | Support for building test expressions and type monomorphiscation.
-- (c) 2014-2017 Vladimír Štill

module Testing.Arguments
    ( getTestableType
    , getDegeneralizedTypes
    , degeneralize
    , buildTestExpression
    , buildTestExpressionsWithComparer
    ) where

import Prelude hiding ( fail )
import Control.Monad ( filterM )
import Control.Monad.Fail ( fail, MonadFail )
import Control.Arrow ( (>>>), first )
import Data.Bool ( bool )
import Data.Set ( Set, toList, fromList )
import Types
import Types.Parser ( parseType )
import Language.Haskell.Interpreter ( MonadInterpreter, typeChecks, typeOf )

-- | Note that polymorphic type (both uncostrained and constrained) can belong
-- to any typeclass, and therefore isTypeclass "a" "AnyClassInScope" returns
-- always true.
isTypeclass :: MonadInterpreter m => Type -> String -> m Bool
isTypeclass ty typeclass = do
    tc <- typeChecks expr
    if not tc
        then return False
        else either (const False) plainType . parseType <$> typeOf expr
  where
    expr = concat [ "[ undefined :: ", formatType ty, ","
                  , " undefined :: ", typeclass, " a => a ]"
                  ]

isTypeclasses :: MonadInterpreter m => Type -> [String] -> m (Either [String] ())
isTypeclasses ty = fmap finalize . filterM (fmap not . (ty `isTypeclass`))
  where
    finalize :: [String] -> Either [String] ()
    finalize [] = Right ()
    finalize xs = Left xs

-- | Get degeneralized (monomorphised) types form type expression (which can
-- be polymoprhic). Uses 'getTestableType' and 'degeneralize'.
getDegeneralizedTypes :: (MonadFail m, MonadInterpreter m) => Bool -> TypeExpression -> m [Type]
getDegeneralizedTypes ignoreRet = fmap degeneralize . getTestableType ignoreRet

-- | Get type expression of testable polymorphic type, or error message if
-- type is not testable. Typeclasses are added to type context to facilitate
-- testability if necessary.
getTestableType :: forall m. (MonadFail m, MonadInterpreter m)
                => Bool -> TypeExpression -> m TypeExpression
getTestableType ignoreRet (TypeExpression (TypeContext ctx) ty) = finalize <$> gtt False ty
  where
    finalize :: Set (TypeClass, [Type]) -> TypeExpression
    finalize ctxnew = TypeExpression (TypeContext . toList $ ctxnew `mappend` fromList ctx) ty
    
    gtt :: Bool -> Type -> m (Set (TypeClass, [Type]))
    gtt nested = foldArgumentsM funapp (arg nested)
    funapp a b = return $ a `mappend` b
    arg nested isret typ
        | Just _ <- splitFunApp typ      = gtt True typ
        | Just ts <- unwrapTupleType typ = mconcat <$> mapM (gtt True) ts
        | Just t <- unwrapListType typ   = gtt True t
        | TypeConstructor (TyLit _) <- typ
                                         = return mempty
        | TypeConstructor _ <- typ       = checkTestable
        | TypeVariable var <- typ        = return $ mkTestable var
        | Just (TyCon _, args) <- splitConApp typ
                                         = (mconcat <$> mapM (gtt True) args) >>= \x -> checkTestable >> return x
        | otherwise                      = fail $ "Not testable, don't know how to test `" ++ formatType typ ++ "'."
      where
        checkTestable = isTypeclasses typ classes >>= \case
                            Right _ -> pure mempty
                            Left es -> fail $ "Not testable: `" ++ formatType typ ++ " missing: " ++ show es ++ "'."
        classes
            | not nested && isret = if ignoreRet then [] else [ "Eq", "Show", "NFData" ]
            | nested && not isret = [ "CoArbitrary", "Function" ]
            | otherwise           = ["Arbitrary"]
        mkTestable var = fromList . map (, [TypeVariable var]) $
              "Show" : bool ["Arbitrary"] [ "CoArbitrary", "Function" ] (nested && not isret)
                     ++ ifL ["Eq"] (not nested && isret)

        ifL l True  = l
        ifL _ False = []

-- | Get test expression of type 'AnyProperty' which can be run in interpreter.
buildTestExpression :: forall m. MonadInterpreter m => String -> String -> Type -> m String
buildTestExpression st so ty = do
    (binds, pars) <- ($ ty) $ functionTypes >>> fst >>> zip [0 :: Int ..] >>> mapM (first show >>> uncurry arg) >>> fmap unzip
    return . unwords $ if null binds
        then [ "AnyProperty (", withWitness st, "<==>", withWitness so, ")" ]
        else "AnyProperty (\\" : binds ++ [ "->", withWitness st ] ++
                   pars ++ [ "<==>", withWitness so ] ++ pars ++ [ ")" ]
  where
    withWitness fn = "((" ++ fn ++ ") `withTypeOf` (undefined :: " ++ formatType ty ++ "))"
    arg :: String -> Type -> m (String, String)
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

buildTestExpressionsWithComparer :: String -> String -> String -> String
buildTestExpressionsWithComparer student solution comparer =
      unwords [ "AnyProperty (", comparer, student, solution, ")" ]

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
