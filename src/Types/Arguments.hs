{-# LANGUAGE TupleSections, NamedFieldPuns, PatternGuards #-}

-- (c) 2014, 2015 Vladimír Štill

module Types.Arguments
    ( getTestableArguments
    , degeneralize
    , TestableArgument ( .. )
    ) where

import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.Either
import Types
import Types.Processing
import Types.Parser ( parseType )
import Language.Haskell.Interpreter ( Interpreter, typeChecks, typeOf )

data TestableArgument = TestableArgument
    { qualifiedType :: TypeExpression
    , bindGen       :: Int -> String -- lambda binding pattern generator
    , varGen        :: Int -> String -- variable name generator
    }

-- | Note that polymorphic type (both uncostrained and constrained) can belong
-- to any typeclass, and therefore isTypeclass "a" "AnyClassInScope" returns
-- always true.
isTypeclass :: TypeExpression -> String -> Interpreter Bool
isTypeclass (TypeExpression con ty) typeclass = do
    tc <- typeChecks expr
    if not tc
        then return False
        else either (const False) plainType . parseType <$> typeOf expr
  where
    expr = concat [ "[ undefined :: ", formatType ty, ","
                  , " undefined :: ", typeclass, " a => a ]"
                  ]

isEq = (`isTypeclass` "Eq")
isArbitrary x = liftM2 (&&) (x `isTypeclass` "Arbitrary")  (x `isTypeclass` "Show")


-- | This function creates list of TestableArguments from (qualified, polymorphic)
-- type. Type can contain higher-order functions.
-- Result is either Left errorMessage if some of the agruments cannot meet
-- Arbitrary typeclass or result does not meet Eq.
-- If everything is OK, Right arguments will be returned, containing argument
-- descriptions for all arguments.
-- - All qualified types of argumens are fully simplified.
-- - Additional constraint for return type being Eq is added if return type is polymorphic
getTestableArguments :: TypeExpression -> Interpreter (Either String [ TestableArgument ])
getTestableArguments (TypeExpression con' typ) = isEq returnType >>= \x -> if x
    then getTestableArguments' con arguments
    else return . Left $ "Return type `" ++ formatType returnType ++ "' not member of Eq"
  where
    returnType = normalize . TypeExpression con $ returnType'
    (arguments, returnType') = functionTypes typ

    -- add extra Eq quantification for any type variable in result type
    con :: TypeContext
    con = if isPolymorphic returnType'
            then TypeContext (map (("Eq", ) . (:[]) . TypeVariable) (typeVars returnType') ++ (\(TypeContext x) -> x) con')
            else con'

functionTypes :: Type -> ([Type], Type)
functionTypes typ
    | isJust (splitFunApp typ) = (init as, last as)
    | otherwise = ([], typ)
  where
    as = doargs typ
    doargs t = case splitFunApp t of
                  Nothing     -> [t] -- return type
                  Just (a, b) -> a : doargs b

getTestableArguments' :: TypeContext -> [ Type ] -> Interpreter (Either String [ TestableArgument ])
getTestableArguments' con ts = fix <$> foldM accum (Right []) ts
  where
    accum done now =
        let a  = argument now
            at = qualifiedType a
            b  = blind now
            bt = qualifiedType b
        in isArbitrary at >>= \arb -> isArbitrary bt >>= \bl ->
            return $ case (arb || bl, arb, done) of
                (True, True,  Right as) -> Right $ a : as
                (True, False, Right as) -> Right $ b : as
                (True,  _, Left msg) -> Left msg
                (False, _, Right _)  -> Left $ msgof at
                (False, _, Left msg) -> Left $ msg ++ ", " ++ msgof at
    blind typ = let
        qualifiedType = normalize $ TypeExpression con
                          (TypeConstructor (TyCon "Blind") `TypeApplication` typ)
        bindGen i = parens $ "Blind " ++ varGen i
        varGen i  = "b" ++ show i
        in TestableArgument { qualifiedType, bindGen, varGen }

    argument typ 
      | Just (a, b) <- splitFunApp typ = let
            (par, re) = functionTypes typ
            bindGen i = parens $ "Fun _" ++ show i ++ " f" ++ show i
            in if length par == 1
                then let
                    qualifiedType = normalize $ TypeExpression con 
                        ((TypeConstructor (TyCon "Fun") `TypeApplication` a) `TypeApplication` b)
                    varGen i = "f" ++ show i
                    in TestableArgument { qualifiedType, bindGen, varGen }
                else let
                    qualifiedType = normalize $ TypeExpression con
                        ((TypeConstructor (TyCon "Fun") `TypeApplication` tupleType par) `TypeApplication` re)
                    varGen i = "(gcurry f" ++ show i ++ ")"
                    in TestableArgument { qualifiedType, bindGen, varGen }
      | otherwise = let
            qualifiedType = normalizeAndSimplify $ TypeExpression con typ
            bindGen = parens . varGen
            varGen i = "x" ++ show i
            in TestableArgument { qualifiedType, bindGen, varGen }

    msgof typ = "type `" ++ formatType typ ++ "' is not instance of Arbitrary class and cannot be tested" 

    fix (Right xs) = Right $ reverse xs
    fix l          = l

    parens = ('(' :) . (++ ")")

-- | This function generates monomorphic instances out of possibly polymorphic
-- arguments
--
-- TODO: Actually select proper types and possibly generate more instances,
-- not just degeneralize every type variable to Integer
degeneralize :: TypeExpression -> [ TypeExpression ]
degeneralize ty = [ ty // degenSubst ]
  where
    degenSubst = map (\x -> (x, TypeConstructor (TyCon "Integer"))) vars
    vars = typeVars ty
