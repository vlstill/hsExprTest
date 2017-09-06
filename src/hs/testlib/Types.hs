{-# LANGUAGE DeriveDataTypeable, TupleSections, LambdaCase, PatternGuards #-}

-- | Runtime representation of Haskell data types, together with
-- formating and manipulation (unification, decomposition and creation).
--
-- * (c) 2014,2015 Vladimír Štill
-- * (c) 2012 Martin Jonáš

module Types (
      -- * Type representation
      TypeExpression(..)
    , TypeContext(..)
    , Type(..)
    , TypeClass
    , TypeVar
    , TypeConstr(..)
    -- *
    , CType(..)
    -- * substitution
    , Substitution
    , (//)
    , isTrivial
    -- * Unification
    , unifyTypes
    -- * functions for construction and destruction of types
    -- ** Folds
    , foldType
    , foldArguments
    , foldArguments'
    , foldArgumentsM
    -- ** Type queriing
    , isPolymorphic
    , isFunction
    , isHigherOrderFunction
    , splitFunApp
    , splitConApp
    , functionTypes
    , returnType
    -- ** type construction
    , tupleType
    , unwrapTupleType
    , unwrapListType
    , (-->)
    -- * Comparing
    , expressionsEqual
    , TypeOrdering (..)
    , addImpliedOrderings
    , compareTypes
    -- * Formating
    , FormatType ( formatType )
    , formatContext
    ) where


import Control.Arrow
import Control.Monad
import Data.Maybe
import Data.List
import Data.Data ( Data )
import Data.Typeable ( Typeable )
import Data.Function ( on )
import Text.PrettyPrint

-- | Represents data type expression, with type context.
data TypeExpression = TypeExpression { getTypeContext :: TypeContext
                                     , getType :: Type
                                     }
    deriving ( Show, Read, Typeable, Data )

-- | Represents data type context. Multiparameter type classes, and
-- generalized contexts (such as @Show (Foo a)@) are supported.
data TypeContext = TypeContext [(TypeClass, [Type])]
    deriving ( Show, Read, Typeable, Data )

-- | Represents data type witnout the type context. Type literals
-- are supported.
data Type = TypeApplication Type Type -- ^ Application of the (partially applied)
            -- type constructor or variable on another type
          | TypeConstructor TypeConstr
          | TypeVariable TypeVar
          deriving ( Show, Read, Eq, Ord, Typeable, Data )

type TypeClass = String
type TypeVar = String

-- | Type costructors: build-in type constructors @(->)@, @[]@, @(,)@, @(,,)@…
-- are handled separatelly to allow for better analysis of those cases.
-- Furthermore type literals are distinct from type user/library defined
-- type constructors. Type constructor arity is not tracked, except for tuple
-- type.
data TypeConstr = FunTyCon -- @(->)@
                | ListTyCon -- @[]@
                | TupleTyCon Int -- ^ tuple with arity
                | TyLit String -- ^ type literal (@-XDataKinds@)
                | TyCon String -- ^ any other type constructor
                deriving ( Show, Read, Eq, Ord, Typeable, Data )

-- | Katamorphism of 'TypeConstr'.
foldType :: (a -> a -> a) -- ^ TypeApplication
         -> (TypeConstr -> a) -- ^ TypeConstructor
         -> (TypeVar -> a) -- TypeVariable
         -> Type -> a
foldType tyApp tyCon tyVar = ftgo
  where
    ftgo (TypeApplication t1 t2) = ftgo t1 `tyApp` ftgo t2
    ftgo (TypeConstructor t)     = tyCon t
    ftgo (TypeVariable v)        = tyVar v

-- | Returns 'True' if given type contains any type variable.
isPolymorphic :: Type -> Bool
isPolymorphic = foldType (||) (const False) (const True)

-- | Returns 'True' if given type is function type
isFunction :: Type -> Bool
isFunction fn
    | Just _ <- splitFunApp fn = True
    | otherwise = False

-- | Return 'True' if given type is higher order functon, that is any of
-- its parameters are function. If type is not function type, 'False' is
-- returned.
isHigherOrderFunction :: Type -> Bool
isHigherOrderFunction = any isFunction . fst . functionTypes

-- | Substitution is mapping from type variables to types.
type Substitution = [(TypeVar, Type)]

-- | Common queriing functions for 'Type' and 'TypeExpression'
class CType t where
    substitute :: Substitution -> t -> t
    -- | Type variables in order of first occurrence in type (first in type and
    -- then in context for TypeExpression)
    typeVars :: t -> [TypeVar]
    -- | Type is plain if all type context constraints are of form
    -- @Class tyVar1 … tyVarN@, or if it is not polymorphic.
    plainType :: t -> Bool

-- | infix operator equivalent to @'flip' 'substitute'@
(//) :: CType t => t -> Substitution -> t
(//) = flip substitute

-- | type has no type variables
fullyInstantiated :: CType t => t -> Bool
fullyInstantiated = null . typeVars

instance CType Type where
    substitute sub = sgo
      where
        sgo = foldType (TypeApplication `on` sgo) TypeConstructor subst
        subst :: TypeVar -> Type
        subst x = fromMaybe (TypeVariable x) (x `lookup` sub)
    typeVars = nub . foldType (++) (const []) (:[])
    plainType _ = True

instance CType TypeContext where
    substitute sub (TypeContext con) = TypeContext scon
      where
        scon = mapMaybe (second (map (substitute sub)) >>> maybeDrop) con
        maybeDrop (c, t)
          | all fullyInstantiated t = Nothing
          | otherwise               = Just (c, t)
    typeVars (TypeContext con) = nub $ concatMap (concatMap typeVars . snd) con
    plainType (TypeContext con) = all (all (foldType (\_ _ -> False) (const False) (const True)) . snd) con

instance CType TypeExpression where
    substitute sub (TypeExpression con ty) = TypeExpression (substitute sub con) (substitute sub ty)

    -- note: order is important here: in normalization, we reauire that variables are
    -- sorted by first occurence in type, not in context as that can have arbitrary
    -- order
    typeVars (TypeExpression con ty) = nub $ typeVars ty ++ typeVars con
    plainType (TypeExpression con ty) = plainType con && plainType ty

-- | split type application, for type of form @a -> b@ returns @Just (a, b)@, othrerwise 'Nothing'
splitFunApp :: Type -> Maybe (Type, Type)
splitFunApp ((TypeConstructor FunTyCon `TypeApplication` arg1) `TypeApplication` arg2) = Just (arg1, arg2)
splitFunApp _ = Nothing

-- | split type constructor application
splitConApp :: Type -> Maybe (TypeConstr, [Type])
splitConApp (TypeConstructor con) = Just (con, [])
splitConApp (TypeApplication a b)
    | Just (con, ts) <- splitConApp a = Just (con, ts ++ [b])
    | otherwise = Nothing
splitConApp _ = Nothing

-- | Return types of parameter and return type of functions
functionTypes :: Type -> ([Type], Type)
functionTypes typ = case foldArguments' (++) (:[]) typ of
    [t] -> ([], t)
    ts  -> (init ts, last ts)

-- | Get return type of function
returnType :: Type -> Type
returnType = foldArguments' (flip const) id

-- | Fold on structure of function type.
foldArguments :: (a -> a -> a) -- ^ (->) application
              -> (Bool -> Type -> a)   -- ^ argument processing (Bool value
                                       -- indicates if this is return type
              -> Type -> a
foldArguments fapp farg = go
  where
    go ty
      | Just (a, b) <- splitFunApp ty = farg False a `fapp` go b
      | otherwise = farg True ty

-- | simplified version of 'foldArguments' which lacks indication of
-- return type in second callback
--
-- @id = foldArguments' (-->) id
foldArguments' :: (a -> a -> a) -> (Type -> a) -> Type -> a
foldArguments' fapp farg = foldArguments fapp (const farg)

-- | Monadic version of 'foldArguments'
foldArgumentsM :: Monad m => (a -> a -> m a)
                          -> (Bool -> Type -> m a)
                          -> Type -> m a
foldArgumentsM fapp = foldArguments (\a b -> join $ liftM2 fapp a b)

-- | Function arrow operator of symbolic types.
(-->) :: Type -> Type -> Type
a --> b = (TypeConstructor FunTyCon `TypeApplication` a) `TypeApplication` b
infixl -->

-- | Return tuple type for given parameters.
tupleType :: [Type] -> Type
tupleType args = foldl TypeApplication (TypeConstructor (TupleTyCon n)) args
  where
    n = length args

-- | Inversion of 'tupleType', returns 'Nothing' if given type is
-- not tuple type.
unwrapTupleType :: Type -> Maybe [Type]
unwrapTupleType = untuple []
  where
    untuple ts (TypeConstructor (TupleTyCon n))
        | length ts == n = Just ts
        | otherwise      = Nothing
    untuple ts (TypeApplication r t) = untuple (t:ts) r
    untuple _ _ = Nothing

-- | Uf input type is of form @[a]@ return @a@, otherise 'Nothing'.
unwrapListType :: Type -> Maybe Type
unwrapListType (TypeApplication (TypeConstructor ListTyCon) t) = Just t
unwrapListType _ = Nothing

-- | Unification is pair of substiturions, one for each unified type.
type Unification = (Substitution, Substitution)

-- | Unify two types, returns most general unification, that is
-- when @(s1, s2) = unifyTypes t1 t2@ then @t1 // s1@ is equal to @t2 // s2@.
--
-- Type variables in types are prefixed with @"a_"@ for first type and
-- @"b_"@ for second.
unifyTypes :: Type -> Type -> Either String Unification
unifyTypes ta tb = (,) <$> ((`composeSubst` tas) <$> mgu) <*> ((`composeSubst` tbs) <$> mgu)
  where
    mgu :: Either String Substitution
    mgu = ((ta // tas) `unsafeUnify` (tb // tbs)) >>= substitutionClosure
    tas = addPrefixSubst "a_" ta
    tbs = addPrefixSubst "b_" tb

-- | internal
substitutionClosure :: Substitution -> Either String Substitution
substitutionClosure = sort >>> nub >>> groupBy ((==) `on` fst) >>> mapM merge >=> clo
  where
    merge :: Substitution -> Either String (TypeVar, Type)
    merge ((va, ta):(_, tb):xs) = ta `unsafeUnify` tb >>= substitutionClosure >>=
                                  merge . (:xs) . (va, ) . (ta //)
    merge [x] = Right x
    merge [] = error "merge: empty list"

    clo :: Substitution -> Either String Substitution
    clo sub0 = mapM (\(v, t) -> (v, ) <$> unroll [v] t) sub
      where
        -- first we have to canonize equality chains, therefore we calculate
        -- equality clasees and then change any variable-to-variable substitution
        -- so that it substitutes to lexicographically first variable in each
        -- of the classes
        vars = map fst sub0
        triv = ($ sub0) $
               filter (\(_, t) -> case t of TypeVariable _ -> True; _ -> False) >>>
               map (\(v, TypeVariable t) -> (v, t))
        classes = ($ vars) $ map (:[]) >>> cloop >>> map (filter (`elem` vars))
        cloop vars0 = let vars1 = ($ vars0) $
                                  map (concatMap (\v -> v : map snd (filter ((== v) . fst) triv))) >>>
                                  map (sort >>> nub) >>> sort >>> nub
                      in if vars1 == vars0 then vars0
                                           else cloop vars1
        sub = ($ sub0) $ map standardize >>> filter (\(t, v) -> case v of TypeVariable tt -> t /= tt; _ -> True)
        standardize (v, TypeVariable _) = (v, TypeVariable . head . fromJust $ find (v `elem`) classes)
        standardize x = x

        -- when we have such a processes substitution, we can unroll it fully
        -- and perform occurs check
        unroll :: [TypeVar] -> Type -> Either String Type
        unroll [] = error "unroll: empty list"
        unroll seen@(v:_) = foldType (liftM2 TypeApplication) (return . TypeConstructor) unrollTyVar >=> \tt ->
                            case tt of
                                TypeVariable _ -> Right tt
                                _ | tt `contains` v ->
                                        Left $ "Unification error: occurs check `" ++ v ++
                                        "' in `" ++ formatType tt ++ "'"
                                  | otherwise -> Right tt
          where
            unrollTyVar :: TypeVar -> Either String Type
            unrollTyVar var
              | var `elem` seen          = Right $ TypeVariable var
              | Just t <- lookup var sub = unroll (var:seen) t
              | otherwise = Right $ TypeVariable var
    contains ty var = foldType (||) (const False) (== var) ty

-- | Add specified prefix to all type variables in type.
addPrefixSubst :: CType t => String -> t -> Substitution
addPrefixSubst prefix = typeVars >>> map (id &&& ((prefix ++) >>> TypeVariable))

-- | @t // composeSubst s1 s2 = (t // s1) // s2@
composeSubst :: Substitution -> Substitution -> Substitution
composeSubst s2 = map (second (// s2))

unsafeUnify :: Type -> Type -> Either String Substitution
unsafeUnify (TypeApplication ta1 ta2) (TypeApplication tb1 tb2) =
    (++) <$> (ta1 `unsafeUnify` tb1) <*> (ta2 `unsafeUnify` tb2)
unsafeUnify tca@(TypeConstructor ca) tcb@(TypeConstructor cb)
    | ca == cb  = Right []
    | otherwise = Left $ "Unification error: type constructor mismatch (" ++
                  "could not match `" ++ formatType tca ++ "' with `" ++ formatType tcb ++ "')"
-- avoid breaking symmetry of equality of type variables
unsafeUnify tva@(TypeVariable va) tvb@(TypeVariable vb) = Right [(va, tvb), (vb, tva)]
unsafeUnify (TypeVariable var) anything = Right [(var, anything)]
unsafeUnify anything (TypeVariable var) = Right [(var, anything)]
unsafeUnify ta tb = Left $ "Unification error: could not match `" ++ formatType ta ++
                           "' with `" ++ formatType tb ++ "'"

-- | Is sustitution trivial (injective reanaming of variables)?
isTrivial :: Substitution -> Bool
isTrivial = (&&) <$> all (snd >>> tvar) <*> (sort >>> nub >>> groupBy ((==) `on` snd) >>> all ((== 1) . length))
  where
    tvar :: Type -> Bool
    tvar (TypeVariable _) = True
    tvar _                = False

-- | Compare type context, order does not matter, variable naming does.
instance Eq TypeContext where
    (==) (TypeContext a) (TypeContext b) = sort a == sort b

-- | Compare type expressions, same as 'expressionsEqual'.
instance Eq TypeExpression where
    (==) = expressionsEqual

-- | Compare type expressions for equality (same as '(==)'). It uses
-- 'compareTypes' internally and checks for 'TypeOrdering' 'Equal'.
expressionsEqual :: TypeExpression -> TypeExpression -> Bool
expressionsEqual a b = (== Equal) . fst $ compareTypes a b

-- | Type order based on unification result.
data TypeOrdering = Equal | MoreGeneral | LessGeneral | Unifiable | NotUnifiable
                    deriving (Eq, Show, Read)

-- | add implied ordering to list of 'TypeOrdering' (which can be seen as list
-- of "one of orderings which should apply". Result is deduplicated.
-- More specifically, if list contains 'Unifiable' result will also contain
-- 'Equal', 'MoreGeneral', 'LessGeneral'.
addImpliedOrderings :: [TypeOrdering] -> [TypeOrdering]
addImpliedOrderings ord
  | Unifiable `elem` ord = nub (ord ++ [Equal, MoreGeneral, LessGeneral])
  | otherwise            = nub ord

-- | Compare type expressions, return 'TypeOrdering' and string message with
-- humar readable description.
compareTypes :: TypeExpression -> TypeExpression -> (TypeOrdering, String)
compareTypes ea@(TypeExpression ca ta) eb@(TypeExpression cb tb) =
    case unifyTypes ta tb of
        Right (mgua, mgub) -> case (isTrivial mgua, isTrivial mgub, cas == cbs) of
            (True, True, True)  -> (Equal, "Types equal.")
            (True, True, False) -> case (casl `subset` cbsl, cbsl `subset` casl) of
                (True, False) -> (MoreGeneral, "First type context `" ++ formatContext ca ++
                                               "' is more permissive then second `" ++
                                               formatContext cb ++ "'.")
                (False, True) -> (LessGeneral, "First type context `" ++ formatContext ca ++
                                               "' is less permissive then second `" ++
                                               formatContext cb ++ "'.")
                _             -> (NotUnifiable, "Type context mismatch: `" ++ formatContext ca ++
                                                "' /= `" ++ formatContext cb ++ "'.")
            (False, True, _)    -> (MoreGeneral, "First type `" ++ formatType ea ++
                                                 "' is more general then second `" ++
                                                 formatType eb ++ "'.")
            (True, False, _)    -> (LessGeneral, "First type `" ++ formatType ea ++
                                                 "' is less general then second `" ++
                                                 formatType eb ++ "'.")
            _                   -> (Unifiable, "Types are neither equal, not one more general then other. However, they are unifiable.")
          where
            cas@(TypeContext casl) = ca // mgua
            cbs@(TypeContext cbsl) = cb // mgub
        Left emsg               -> (NotUnifiable, "Types are neither equal, nor unifiable: " ++ emsg)

subset :: Eq a => [a] -> [a] -> Bool
subset a b = all (`elem` b) a

-- | Human readable representation of type.
class FormatType t where
    formatType :: t -> String

data Arg = Error
         | End
         | Val String
         | Fun (Maybe String -> Arg)


apply :: Arg -> Arg -> Arg
apply (Fun f)   (Val x) = f (Just x)
apply (Fun f)   End     = f Nothing
apply f@(Fun _) (Fun g) = f `apply` g Nothing
apply _       _ = Error

unwrap :: Arg -> String
unwrap (Val x) = x
unwrap (Fun f) = unwrap (f Nothing)
unwrap x       = error $ "formatType: Invalid type " ++ case x of
                                                            Error -> "(Error)"
                                                            End   -> "(End)"
                                                            _     -> error "Unhandled error"

instance FormatType Type where
    formatType = unwrap . foldType apply formatCon apcon
      where
        formatCon :: TypeConstr -> Arg
        formatCon FunTyCon = Fun $ \case
                              Nothing -> Val "(->)"
                              Just x  -> Fun $ \case
                                  Nothing -> Val $ "(->) " ++ _parens x
                                  Just y  -> Val $ _parens' ("->" `isInfixOf`) x ++ " -> " ++ y
        formatCon ListTyCon = Fun $ \case
                                Nothing -> Val "[]"
                                Just x -> Val $ "[" ++ x ++ "]"
        formatCon (TupleTyCon n) = aptuple [] n
        formatCon (TyLit con) = apcon con
        formatCon (TyCon con) = apcon con

        aptuple :: [String] -> Int -> Arg
        aptuple args 0 = Val $ "(" ++ intercalate ", " args ++ ")"
        aptuple args n = Fun $ \case
                            Nothing -> Val $ "(" ++ replicate (n + length args - 1) ',' ++ ")" ++
                                        if null args then "" else " " ++ unwords (map _parens args)
                            Just v  -> aptuple (args ++ [v]) (n - 1)
        apcon :: String -> Arg
        apcon con = Fun $ \case
                      Nothing -> Val con
                      Just x  -> apcon $ con ++ " " ++ _parens x

instance FormatType TypeExpression where
    formatType (TypeExpression (TypeContext []) ty) = formatType ty
    formatType (TypeExpression con ty) = formatContext con ++ " => "++ formatType ty

instance PrettyPrint Type where pp = formatType
instance PrettyPrint TypeExpression where pp = formatType
instance PrettyPrint TypeContext where pp = formatContext

-- | Format type context, empty context is formated as @"()"@
formatContext :: TypeContext -> String
formatContext (TypeContext []) = "()"
formatContext (TypeContext [(c, v)]) = c ++ " " ++ _formatTList v
formatContext (TypeContext cs) = _tuple $ map (\(c, v) -> c ++ " " ++ _formatTList v) cs

_formatTList :: [Type] -> String
_formatTList = unwords . map (_parens . formatType)

_parens' :: (String -> Bool) -> String -> String
_parens' p x
    | head x == '(' && last x == ')' = x
    | p x                            = '(' : x ++ ")"
    | otherwise                      = x

_parens :: String -> String
_parens = _parens' (' ' `elem`)

_tuple :: [String] -> String
_tuple x = '(' : intercalate ", " x ++ ")"
