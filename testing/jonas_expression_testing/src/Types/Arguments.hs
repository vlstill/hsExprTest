{-# LANGUAGE TupleSections
           , NamedFieldPuns
           #-}
module Types.Arguments
    ( getTestableArguments
    , degeneralize
    , TestableArgument ( .. )
    ) where

import Control.Monad
import Types.TypeExpression
import Types.Formating 
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
isTypeclass typ typeclass = do
    tc <- typeChecks expr
    if not tc
        then return False
        else do
            rtype <- typeOf expr
            case parseType rtype of
                Left _  -> return False
                Right _ -> return True
  where
    expr = concat [ "(undefined :: ", formatType gtype, ")"
                  , " . "
                  , "(undefined :: ", typeclass, " a => a -> a)"
                  ]
    gtype = case typ of
        TypeExpression con ty -> TypeExpression con (ty `FunctionType` TupleType [])


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
getTestableArguments (TypeExpression con' typ) = isEq returnType >>= \x -> case x of
    True  -> getTestableArguments' con arguments
    False -> return . Left $ "Return type `" ++ formatType returnType ++ "' not member of Eq"
  where
    returnType = normalize . TypeExpression con $ returnType'
    returnType' = rt typ
    rt (FunctionType _ t) = rt t
    rt t                  = t

    -- add extra Eq quantification for any type variable in result type
    con :: TypeContext
    con = if isPoly returnType'
            then TypeContext (map ("Eq", ) (getVars returnType') ++ (\(TypeContext x) -> x) con')
            else con'

    getVars :: Type -> [ TypeVariable ]
    getVars = extract (++) [] (:[])

    arguments = as typ
    as (FunctionType a b) = a : as b
    as return             = []

isPoly :: Type -> Bool
isPoly = extract (||) False (const True)

getTestableArguments' :: TypeContext -> [ Type ] -> Interpreter (Either String [ TestableArgument ])
getTestableArguments' con ts = foldM accum (Right []) ts >>= return . fix
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
                          (TypeConstructor "Blind" `TypeApplication` typ)
        bindGen i = parens $ "Blind " ++ varGen i
        varGen i  = "b" ++ show i
        in TestableArgument { qualifiedType, bindGen, varGen }

    argument (FunctionType a b) = let
        qualifiedType = normalize $ TypeExpression con 
                          ((TypeConstructor "Fun" `TypeApplication` a) `TypeApplication` b)
        bindGen i = parens $ "Fun _" ++ show i ++ " " ++ varGen i
        varGen  i = "f" ++ show i
        in TestableArgument { qualifiedType, bindGen, varGen }

    argument typ = let
        qualifiedType = normalize $ TypeExpression con typ
        bindGen = parens . varGen
        varGen i = "x" ++ show i
        in TestableArgument { qualifiedType, bindGen, varGen }

    msgof typ = "type `" ++ formatType typ ++ "' is not instance of Arbitrary class and cannot be tested" 

    fix (Right xs) = Right $ reverse xs
    fix l          = l

    parens = ('(' :) . (++ ")")

foldType :: (b -> b -> b) -- ^ TypeApplication
         -> (String -> b) -- ^ TypeConstructor
         -> (b -> b -> b) -- ^ FunctionType
         -> (String -> b) -- ^ VariableType
         -> ([b] -> b)    -- ^ TupleType
         -> (b -> b)      -- ^ ListType
         -> Type -> b
foldType ta tc ft vt tt lt typ = let self = foldType ta tc ft vt tt lt in case typ of
    TypeApplication a b -> ta (self a) (self b)
    TypeConstructor c   -> tc c
    FunctionType a b    -> ft (self a) (self b)
    VariableType var    -> vt var
    TupleType ts        -> tt (map self ts)
    ListType  l         -> lt (self l)

extract :: (b -> b -> b) -> b -> (String -> b) -> Type -> b
extract binop ide tyvar = foldType binop (const ide) binop tyvar (foldr binop ide) id


-- | This function generates monomorphic instances out of possibly polymorphic
-- arguments
--
-- TODO: Actually select proper types and possibly generate more instances,
-- not just degeneralize every type variable to Integer
degeneralize :: [ TestableArgument ] -> [ [ TestableArgument ] ]
degeneralize = (:[]) . map degen
  where
    degen t@(TestableArgument { qualifiedType = TypeExpression con typ }) =
        t { qualifiedType = normalize . TypeExpression con $
              foldType TypeApplication TypeConstructor FunctionType
                       (const $ TypeConstructor "Integer") TupleType ListType typ }
