{-# LANGUAGE TemplateHaskell, TupleSections, LambdaCase, DeriveLift #-}

-- | Functions for working with Template Haskell type representation.
--
-- (c) 2018 Vladimír Štill

module Test.Expr.Types ( arity, uncurryType, isFunctionType, hasInstance, normalizeContext
                       , getTVars, regenTVars
                       -- * Type Substitution
                       , Substitution, substitute, (//)
                       , Renaming, rename
                       -- * Unification
                       , UniTypeId, TypeOrder, unify, unifyingSubstitution
                       ) where

import Language.Haskell.TH ( Q, Type (..), Name, reifyInstances, TyVarBndr (..), Cxt, newName, pprint )
import Language.Haskell.TH.Syntax ( Lift )
import Language.Haskell.TH.ExpandSyns ( substInType )
import Data.List ( foldl', nub )
import Data.Foldable ( toList )
import Data.Semigroup ( (<>) )
import Data.Monoid ( mempty )
import Text.Printf.Mauke.TH ( sprintf )

import           Data.Set        ( Set )
import qualified Data.Set as Set ( singleton, member )
import qualified Data.Map as Map ( singleton, lookup )

-- | Get arity of a type (i.e. the number of arguments needed to make the value
-- fully applied).
--
-- $('showQ' $ arity <$> [t| forall a . a -> (a -> a) -> (Int -> Int) |])@
arity :: Type -> Int
arity t = length (fst (uncurryType t))

-- | Get a list of types of all arguments and the return type. All context are
-- stripped.
uncurryType :: Type -> ([Type], Type)
uncurryType t0 = let t = unct t0 in (init t, last t)
  where
    unct (AppT (AppT ArrowT t1) t2) = t1 : unct t2
    unct (ForallT _ _ ty) = unct ty
    unct x = [x]

-- | Is the top-level type constructor a fully applied (->)?
isFunctionType :: Type -> Bool
isFunctionType (AppT (AppT ArrowT _) _) = True
isFunctionType _                        = False

-- | Does given type have instance of given class?
hasInstance :: Type -> Name -> Q Bool
hasInstance t cls = (== 1) . length <$> reifyInstances cls [t]

-- | Merge type contexts which can be merged. Note: this functions works
-- correctly only if none of the quantifiers shadows any type variable.
-- This seems to be the case both for reified types (where multiple contexts
-- occur for class methods) and for @[t| … |]@ expressions.
normalizeContext :: Type -> Type
normalizeContext = set . nc
  where
    set :: ([TyVarBndr], Cxt, Type) -> Type
    set ([], [], t)    = t
    set (bndr, cxt, t) = ForallT bndr cxt t

    nc :: Type -> ([TyVarBndr], Cxt, Type)
    nc (ForallT bndr cxt t0) = (bndr ++ bndrs, cxt ++ cxts, t)
      where
        (bndrs, cxts, t) = nc t0
    nc (AppT (AppT ArrowT l) r) = (bndrs, cxts, AppT (AppT ArrowT nl) nr)
      where
        nl = normalizeContext l
        (bndrs, cxts, nr) = nc r
    nc t = ([], [], t)

-- | Type substitution. Please note that substitution is ordered, i.e.
-- @s1 = [(a, VarT b), (b, ConT Int)]@ is different from
-- @s2 = [(b, ConT Int), (a, VarT b)]@ as for @a -> b // s1@ produces
-- @Int -> Int@ while @a -> b // s2@ produces @b -> Int@.
type Substitution = [(Name, Type)]

-- | Renaming of type variables. Similar to 'Substitution', Renaming is ordered.
type Renaming = [(Name, Name)]

substitute :: Substitution -> Type -> Type
substitute sub t = foldl' (flip substInType) t sub

(//) :: Type -> Substitution -> Type
infixl 9 //
t // sub = substitute sub t

data UniTypeId = LeftType | RightType | BothTypes
                 deriving (Eq, Show, Read, Lift)

data TypeOrder = TUnifiable | TLessGeneral | TMoreGeneral | TEqual
                 deriving (Eq, Show, Read, Lift)

-- | Unify two types
--
-- There are certain limitations, i.e. some unifyable types will not be unified:
--   * higher rank types (nested foralls) are not supported
--   * explicit parenthesis in types are not supported
--   * explicit kind signatures will work only it they are exactly matching
--     (i.e. type variable with kind signature cannot be unified with a
--     constructor or a variable without kind signature)
--   * quantified type variables have proper kind signatures only if they had
--     them in both of the original types
unify :: Type -> Type -> Either (UniTypeId, String) (TypeOrder, Type)
unify lt rt = applyUnification <$> unifyingSubstitution lt rt
  where
    (lbndrs, lcxt, nlt) = splitCxt $ normalizeContext lt
    (rbndrs, rcxt, _  ) = splitCxt $ normalizeContext rt

    splitCxt (ForallT bndrs cxt t) = (bndrs, cxt, t)
    splitCxt t                     = ([], [], t)

    lvars = getTVars lt
    rvars = getTVars rt

    kindmap = mconcat (map toMap (lbndrs <> rbndrs))

    toMap (PlainTV _)    = mempty
    toMap (KindedTV n k) = Map.singleton n k

    fromMap = foldr (\x xs -> toBndr x : xs) []

    toBndr var = case Map.lookup var kindmap of
                    Just k  -> KindedTV var k
                    Nothing -> PlainTV var

    applyUnification :: Substitution -> (TypeOrder, Type)
    applyUnification subst = (typeOrd `meet` cxtOrd, ty)
      where
        basety = nlt // subst
        vars = getTVars basety

        slcxt = nub $ map (// subst) lcxt
        srcxt = nub $ map (// subst) rcxt
        scxt  = nub $ slcxt <> srcxt

        bndrs = fromMap vars

        ty = case (bndrs, scxt) of
            ([], []) -> basety
            _        -> ForallT bndrs scxt basety

        restrict :: Set Name -> Substitution -> Substitution
        restrict varnames = filter ((`Set.member` varnames) . fst)

        isInjective :: Substitution -> Bool
        isInjective xs = length xs == length (nub (map snd xs))

        isRenaming :: Substitution -> Bool
        isRenaming = all (\case VarT _ -> True; _ -> False) . map snd

        ls = restrict lvars subst
        rs = restrict rvars subst

        typeOrd :: TypeOrder
        typeOrd = case (isInjective ls, isInjective rs) of
                (True, True)  | isRenaming subst -> TEqual
                              | otherwise        -> TUnifiable
                (True, False) | isRenaming ls    -> TMoreGeneral
                              | otherwise        -> TUnifiable
                (False, True) | isRenaming rs    -> TLessGeneral
                              | otherwise        -> TUnifiable
                _                                -> TUnifiable

        -- a meet on the lattice of TypeOrder
        meet :: TypeOrder -> TypeOrder -> TypeOrder
        meet TEqual y = y
        meet x TEqual = x
        meet x      y | x == y    = x
                       | otherwise = TUnifiable

        cxtOrd :: TypeOrder
        cxtOrd = case (slcxt `subset` srcxt, srcxt `subset` slcxt) of
                  (True,  True) -> TEqual
                  -- left context is subset of the right -> the left context is *less* restrictive
                  (True, False) -> TMoreGeneral
                  (False, True) -> TLessGeneral
                  _             -> TUnifiable

        subset :: Cxt -> Cxt -> Bool
        subset l r = all (`elem` r) l

-- | Outputs unification of two types, the types must not share any type variables.
unifyingSubstitution :: Type -> Type -> Either (UniTypeId, String) Substitution
unifyingSubstitution t0 t1 = go t0n t1n
  where
    t0n = stripCxt $ normalizeContext t0
    t1n = stripCxt $ normalizeContext t1

    stripCxt (ForallT _ _ t) = t
    stripCxt t               = t

    go (ForallT _ _ _) _         = Left (LeftType,  "RankNType unification not supported")
    go _ (ForallT _ _ _)         = Left (RightType, "RankNType unification not supported")
    go (ParensT _) _             = Left (LeftType,  "Explicit parenthesis not supported in unification")
    go _ (ParensT _)             = Left (RightType, "Explicit parenthesis not supported in unification")

    go (AppT la lb) (AppT ra rb) = gobin (la, lb) (ra, rb)
    go (SigT lt lk) (SigT rt rk)
        | lk == rk               = go lt rt
        | otherwise              = Left (BothTypes, $(sprintf "kind mismatch in matching kind signatures: %s vs. %s")
                                                     (pprint lk) (pprint rk))
    go (VarT l) r                = occursCheck l r
    go l (VarT r)                = occursCheck r l
    go (InfixT la lo lb) (InfixT ra ro rb)
        | lo == ro               = gobin (la, lb) (ra, rb)
        | otherwise              = Left (BothTypes, $(sprintf "binary operator mismatch: %s vs. %s")
                                                     (pprint lo) (pprint ro))
    go (UInfixT la lo lb) (UInfixT ra ro rb)
        | lo == ro               = gobin (la, lb) (ra, rb)
        | otherwise              = Left (BothTypes, $(sprintf "binary operator mismatch: %s vs. %s")
                                                     (pprint lo) (pprint ro))
    go l r
        | l == r                 = pure []
        | otherwise              = Left (BothTypes, $(sprintf "constructor mismatch: %s vs. %s")
                                                     (pprint l) (pprint r))

    gobin (la, lb) (ra, rb) = go la ra >>= \subst -> (subst <>) <$> go (lb // subst) (rb // subst)

    occursCheck nam typ
        | VarT nam == typ         = pure [(nam, typ)]
        | nam `elem` getTVars typ = Left (BothTypes, $(sprintf "attempt to construct infinite type, occursch check failed: %s ~ %s")
                                                      (pprint nam) (pprint typ))
        | otherwise               = pure [(nam, typ)]

-- | Extract all type variables from a type.
getTVars :: Type -> Set Name
getTVars t0 = go t0
  where
    -- note: we deliberately write all cases verbatim to make compiler warnings
    -- notify us if something is added
    go (ForallT bndrs cxt t) = go t <> mconcat (map go cxt) <> mconcat (map goBndr bndrs)
    go (AppT a b)            = go a <> go b
    go (SigT t _)            = go t
    go (VarT n)              = Set.singleton n
    go (ConT _)              = mempty
    go (PromotedT _)         = mempty
    go (InfixT a _ b)        = go a <> go b
    go (UInfixT a _ b)       = go a <> go b
    go (ParensT t)           = go t
    go (TupleT _)            = mempty
    go (UnboxedTupleT _)     = mempty
    go (UnboxedSumT _)       = mempty
    go ArrowT                = mempty
    go EqualityT             = mempty
    go ListT                 = mempty
    go (PromotedTupleT _)    = mempty
    go PromotedNilT          = mempty
    go PromotedConsT         = mempty
    go StarT                 = mempty
    go ConstraintT           = mempty
    go (LitT _)              = mempty
    go WildCardT             = mempty

    goBndr (PlainTV n)    = Set.singleton n
    goBndr (KindedTV n _) = Set.singleton n

rename :: Renaming -> Type -> Type
rename sub t0 = go t0
  where
    go (ForallT bndrs ctx t) = ForallT (map goBndr bndrs) (map go ctx) (go t)
    go (AppT a b)            = AppT (go a) (go b)
    go (SigT t k)            = SigT (go t) k
    go (VarT n)              = VarT (ren n)
    go t@(ConT _)            = t
    go t@(PromotedT _)       = t
    go (InfixT a op b)       = InfixT (go a) op (go b)
    go (UInfixT a op b)      = UInfixT (go a) op (go b)
    go (ParensT t)           = ParensT (go t)
    go t@(TupleT _)          = t
    go t@(UnboxedTupleT _)   = t
    go t@(UnboxedSumT _)     = t
    go t@ArrowT              = t
    go t@EqualityT           = t
    go t@ListT               = t
    go t@(PromotedTupleT _)  = t
    go t@PromotedNilT        = t
    go t@PromotedConsT       = t
    go t@StarT               = t
    go t@ConstraintT         = t
    go t@(LitT _)            = t
    go t@WildCardT           = t

    goBndr (PlainTV n)    = PlainTV (ren n)
    goBndr (KindedTV n k) = KindedTV (ren n) k

    ren :: Name -> Name
    ren n | Just x <- lookup n sub = x
          | otherwise              = n

regenTVars :: String -> Type -> Q Type
regenTVars v t = do
    let vars = toList $ getTVars t
    subst <- mapM (\x -> (x, ) <$> newName v) vars
    let r = rename subst t
    pure r
