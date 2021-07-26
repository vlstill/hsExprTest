{-# LANGUAGE TemplateHaskell, TupleSections, LambdaCase, DeriveLift, DeriveDataTypeable, Unsafe, CPP #-}

-- | Functions for working with Template Haskell type representation.
--
-- (c) 2018–2021 Vladimír Štill

module Test.Expr.Types ( arity, uncurryType, isFunctionType, hasInstance, normalizeContext
                       , getTVars, regenTVars
                       , rewriteAnnotations, stripAnnotations, unannotate
                       , TestAs
                       -- * Type Substitution
                       , Substitution, substitute, (//)
                       , Renaming, rename
                       -- * Unification
                       , UniTypeId (..), TypeOrder (..), unify, unifyingSubstitution
                       -- * Printing
                       , ppty
                       ) where

import Language.Haskell.TH ( Q, Type (..), Name, reifyInstances, TyVarBndr (..), Cxt
                           , newName, pprint )
import Language.Haskell.TH.Syntax ( Lift, mkName
#if MIN_VERSION_template_haskell(2, 17, 0)
                                 , Specificity ( SpecifiedSpec )
#endif
                                 )
import Language.Haskell.TH.ExpandSyns ( substInType )
import Control.Arrow ( second, (>>>) )
import Control.Monad ( filterM )
import Data.List ( foldl', nub, nubBy )
import Data.Foldable ( toList )
#if ! MIN_VERSION_base(4, 13, 0)
import Data.Semigroup ( (<>) )
import Data.Monoid ( mempty )
#endif
import Data.Function ( on )
import Data.Char ( isSpace )
import Data.PartialOrder ( PartialOrder ( pcompare ) )
import Data.Data ( Data, Typeable )
import Text.Printf.Mauke.TH ( sprintf )

import           Data.Set        ( Set )
import qualified Data.Set as Set ( singleton, member, null )
import qualified Data.Map as Map ( singleton, lookup )

-- | Intended to be used with the teacher's solution:
-- @foo :: a `'TestAs'` Positive a -> b `'TestAs'` Large b -> Bool@
type TestAs α β = α

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
    set ([], [], t)    = t
    set (bndr, cxt, t) = ForallT bndr cxt t

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
                 deriving (Eq, Show, Read, Lift, Data, Typeable)

data TypeOrder = TUnifiable | TLessGeneral | TMoreGeneral | TEqual
                 deriving (Eq, Show, Read, Lift, Data, Typeable)

instance PartialOrder TypeOrder where
    pcompare TUnifiable   TUnifiable   = Just EQ
    pcompare TLessGeneral TLessGeneral = Just EQ
    pcompare TMoreGeneral TMoreGeneral = Just EQ
    pcompare TEqual       TEqual       = Just EQ
    pcompare TUnifiable   _            = Just LT
    pcompare TEqual       _            = Just GT
    pcompare _            TUnifiable   = Just GT
    pcompare _            TEqual       = Just LT
    pcompare _            _            = Nothing


{-# ANN gmap "HLint: ignore" #-}
gmap :: Foldable t => (a -> b) -> t a -> [b]
gmap f = foldr (\x xs -> f x : xs) []

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
--   * type contexts cannot be simplified, so we can get contexts which
--     constraints such as @Functor []@ and types misjudged as 'TUnifiable'
--     instead of the actual ordering
unify :: Type -> Type -> Q (Either (UniTypeId, String) (TypeOrder, Type, Substitution))
unify lt rt = hoistQ $ applyUnification <$> unifyingSubstitution lt rt
  where
    hoistQ (Right x)  = fmap Right x
    hoistQ (Left y) = pure (Left y)

    (lbndrs, lcxt, nlt) = splitCxt $ normalizeContext lt
    (rbndrs, rcxt, _  ) = splitCxt $ normalizeContext rt

    splitCxt (ForallT bndrs cxt t) = (bndrs, cxt, t)
    splitCxt t                     = ([], [], t)

    lvars = getTVars lt
    rvars = getTVars rt

    kindmap = mconcat (map toMap (lbndrs <> rbndrs))

    toMap PlainTV {}    = mempty
#if MIN_VERSION_template_haskell(2, 17, 0)
    toMap (KindedTV n _ k) = Map.singleton n k
#else
    toMap (KindedTV n k) = Map.singleton n k
#endif

    fromMap = gmap toBndr

    toBndr var = case Map.lookup var kindmap of
#if MIN_VERSION_template_haskell(2, 17, 0)
                    Just k  -> KindedTV var SpecifiedSpec k
                    Nothing -> PlainTV var SpecifiedSpec
#else
                    Just k  -> KindedTV var k
                    Nothing -> PlainTV var
#endif

    applyUnification :: Substitution -> Q (TypeOrder, Type, Substitution)
    applyUnification subst = do
        slcxt <- nub <$> filterM constraintNeeded (map (// subst) lcxt)
        srcxt <- nub <$> filterM constraintNeeded (map (// subst) rcxt)
        let scxt  = nub $ slcxt <> srcxt

        let ty = case (bndrs, scxt) of
                ([], []) -> basety
                _        -> ForallT bndrs scxt basety

        let cxtOrd :: TypeOrder
            cxtOrd = case (slcxt `subset` srcxt, srcxt `subset` slcxt) of
                      (True,  True) -> TEqual
                      -- left context is subset of the right -> the left context is *less* restrictive
                      (True, False) -> TMoreGeneral
                      (False, True) -> TLessGeneral
                      _             -> TUnifiable

        pure (typeOrd `meet` cxtOrd, ty, subst)
      where
        basety = nlt // subst
        vars = getTVars basety

        bndrs = fromMap vars

        constraintNeeded :: Type -> Q Bool
        constraintNeeded constraint@(AppT (ConT cls) t)
          | Set.null (getTVars constraint) = not <$> hasInstance t cls
          | otherwise = pure True
        constraintNeeded _ = pure True

        -- | restrict the substitution to the given set of source variables and
        -- make it total for this set
        restrictAndTotalize :: Set Name -> Substitution -> Substitution
        restrictAndTotalize varnames sb = nubBy ((==) `on` fst) $
                                            filter ((`Set.member` varnames) . fst) sb
                                            <> gmap (\x -> (x, VarT x)) varnames

        transitiveClosure :: Substitution -> Substitution
        transitiveClosure sb = let sb1 = map (second (// sb)) sb
                               in if sb == sb1 then sb else transitiveClosure sb1

        isInjective :: Substitution -> Bool
        isInjective xs = length xs == length (nub (map snd xs))

        isRenaming :: Substitution -> Bool
        isRenaming = all (\case VarT _ -> True; _ -> False) . map snd

        isTrivial sb = isInjective sb && isRenaming sb

        tsubst = transitiveClosure subst
        ls = restrictAndTotalize lvars tsubst
        rs = restrictAndTotalize rvars tsubst

        typeOrd :: TypeOrder
        typeOrd = case (isTrivial ls, isTrivial rs) of
                     (True,  True ) -> TEqual
                     (False, True ) -> TMoreGeneral
                     (True,  False) -> TLessGeneral
                     (False, False) -> TUnifiable

        -- a meet on the lattice of TypeOrder
        meet :: TypeOrder -> TypeOrder -> TypeOrder
        meet TEqual y = y
        meet x TEqual = x
        meet x      y | x == y    = x
                      | otherwise = TUnifiable

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

    go ForallT { } _             = Left (LeftType,  "RankNType unification not supported")
    go _ ForallT { }             = Left (RightType, "RankNType unification not supported")
    go (ParensT _) _             = Left (LeftType,  "Explicit parenthesis not supported in unification")
    go _ (ParensT _)             = Left (RightType, "Explicit parenthesis not supported in unification")

    go (AppT la lb) (AppT ra rb) = gobin (la, lb) (ra, rb)
    go (SigT lt lk) (SigT rt rk)
        | lk == rk               = go lt rt
        | otherwise              = Left (BothTypes, $(sprintf "kind mismatch in matching kind signatures: %s vs. %s")
                                                     (ppty lk) (ppty rk))
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
                                                     (ppty l) (ppty r))

    gobin (la, lb) (ra, rb) = go la ra >>= \subst -> (subst <>) <$> go (lb // subst) (rb // subst)

    occursCheck nam typ
        | VarT nam == typ         = pure [(nam, typ)]
        | nam `elem` getTVars typ = Left (BothTypes, $(sprintf "attempt to construct infinite type, occursch check failed: %s ~ %s")
                                                      (pprint nam) (ppty typ))
        | otherwise               = pure [(nam, typ)]

-- | Extract all type variables from a type.
getTVars :: Type -> Set Name
getTVars = go
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
#if MIN_VERSION_template_haskell(2, 15, 0)
    go (AppKindT t _)        = go t
    go (ImplicitParamT _ t)  = go t
#endif
#if MIN_VERSION_template_haskell(2, 16, 0)
    go (ForallVisT bndrs t)  = go t <> mconcat (map goBndr bndrs)
#endif
#if MIN_VERSION_template_haskell(2, 17, 0)
    go MulArrowT             = mempty
#endif

#if MIN_VERSION_template_haskell(2, 17, 0)
    goBndr (PlainTV n _)    = Set.singleton n
    goBndr (KindedTV n _ _) = Set.singleton n
#else
    goBndr (PlainTV n)    = Set.singleton n
    goBndr (KindedTV n _) = Set.singleton n
#endif

rename :: Renaming -> Type -> Type
rename sub = go
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
#if MIN_VERSION_template_haskell(2, 15, 0)
    go (AppKindT t k)        = AppKindT (go t) k
    go (ImplicitParamT s t)  = ImplicitParamT s (go t)
#endif
#if MIN_VERSION_template_haskell(2, 16, 0)
    go (ForallVisT bndrs t)  = ForallVisT (map goBndr bndrs) (go t)
#endif
#if MIN_VERSION_template_haskell(2, 17, 0)
    go t@MulArrowT           = t
#endif

#if MIN_VERSION_template_haskell(2, 17, 0)
    goBndr (PlainTV n f)    = PlainTV (ren n) f
    goBndr (KindedTV n f k) = KindedTV (ren n) f k
#else
    goBndr (PlainTV n)    = PlainTV (ren n)
    goBndr (KindedTV n k) = KindedTV (ren n) k
#endif

    ren :: Name -> Name
    ren n | Just x <- lookup n sub = x
          | otherwise              = n

regenTVars :: String -> Type -> Q Type
regenTVars v t = do
    let vars = toList $ getTVars t
    subst <- mapM (\x -> (x, ) <$> newName v) vars
    let r = rename subst t
    pure r

-- | Print a simplified type, more similar to the type printed by GHCi.
-- There are some serious limitations:
-- * the namespaces of types are stripped, regardless of its correctness
-- * explicit foralls are strippend, regardless of need for kind signatures in them
ppty :: Type -> String
ppty = normalizeContext >>> simplify >>> \case
    ty@ForallT {} -> dropWhile isSpace . drop 1 . dropWhile (/= '.') $ pprint ty
    ty            -> pprint ty
  where
    simplify (ForallT bndrs ctx t) = ForallT (map unkind bndrs) (map simplify ctx) (simplify t)
    simplify (AppT a b)            = AppT (simplify a) (simplify b)
    simplify (SigT t k)            = SigT (simplify t) (simplify k)
    simplify (VarT n)              = VarT n
    simplify (ConT n)              = ConT $ simpName n
    simplify (PromotedT n)         = PromotedT $ simpName n
    simplify (InfixT a op b)       = InfixT (simplify a) (simpName op) (simplify b)
    simplify (UInfixT a op b)      = UInfixT (simplify a) (simpName op) (simplify b)
    simplify (ParensT t)           = ParensT (simplify t)
    simplify x                     = x

    simpName = pprint >>> reverse >>> splitdot >>> reverse >>> mkName

    -- get the last part of namespaced name, correctly handling names with dots in them
    splitdot = span (/= '.') >>> \case
                  ("", b)                         -> '.' : splitdot (drop 1 b)
                  (a, b) | filter (== '.') b == b -> a ++ b
                         | otherwise              -> a ++ drop 1 (takeWhile (== '.') b)

#if MIN_VERSION_template_haskell(2, 17, 0)
    unkind (KindedTV n f _) = PlainTV n f
#else
    unkind (KindedTV n _) = PlainTV n
#endif
    unkind p@PlainTV {}  = p

rewriteAnnotations :: Type -> Type
rewriteAnnotations = snd . unannotate

stripAnnotations :: Type -> Type
stripAnnotations = fst . unannotate

-- | @unannotate t ≈ (stripAnnotations t, rewriteAnnotations t)@
unannotate :: Type -> (Type, Type)
unannotate typ = case typ of
    InfixT l op r  -> goInfix InfixT l op r
    UInfixT l op r -> goInfix UInfixT l op r
    -- (VarT ''TestAs `AppT` a) `AppT` b
    (ConT fn `AppT` l) `AppT` r
      | fn == ''TestAs -> (stripAnnotations l, rewriteAnnotations r)
    fn `AppT` x -> go2 AppT fn x
    t `SigT` k -> go2 SigT t k
#if MIN_VERSION_template_haskell(2, 15, 0)
    t `AppKindT` k -> go2 AppKindT t k
#endif
    ForallT bndrs ctx ty -> let (ubn, rbn) = unzip $ goBndr <$> bndrs
                                (uctx, rctx) = unzip $ unannotate <$> ctx
                                (ut, rt) = unannotate ty
                            in (ForallT ubn uctx ut, ForallT rbn rctx rt)
#if MIN_VERSION_template_haskell(2, 16, 0)
    ForallVisT bndrs ty ->  join2 ForallVisT (unzip $ goBndr <$> bndrs) (unannotate ty)
#endif
    ParensT t -> map2 ParensT $ unannotate t

    v@VarT {} -> dup v
    c@ConT {} -> dup c
    p@PromotedT {} -> dup p
    t@TupleT {} -> dup t
    t@UnboxedTupleT {} -> dup t
    s@UnboxedSumT {} -> dup s
    ArrowT -> dup ArrowT
#if MIN_VERSION_template_haskell(2, 17, 0)
    MulArrowT -> dup MulArrowT
#endif
    EqualityT -> dup EqualityT
    ListT -> dup ListT
    t@PromotedTupleT {} -> dup t
    PromotedNilT -> dup PromotedNilT
    PromotedConsT -> dup PromotedConsT
    StarT -> dup StarT
    ConstraintT -> dup ConstraintT
    l@LitT {} -> dup l
    WildCardT -> dup WildCardT
#if MIN_VERSION_template_haskell(2, 15, 0)
    ImplicitParamT x t -> map2 (ImplicitParamT x) $ unannotate t
#endif
  where
    goInfix ctor l op r
      | op == ''TestAs = (stripAnnotations l, rewriteAnnotations r)
      | otherwise      = go2 (\x -> ctor x op) l r
    go2 ctor l r = join2 ctor (unannotate l) (unannotate r)
    join2 ctor (sl, rl) (sr, rr) = (ctor sl sr, ctor rl rr)
    dup x = (x, x)

    goBndr p@PlainTV {} = dup p
#if MIN_VERSION_template_haskell(2, 17, 0)
    goBndr (KindedTV n f k) = map2 (KindedTV n f) (unannotate k)
#else
    goBndr (KindedTV n k) = map2 (KindedTV n) (unannotate k)
#endif
    map2 fn (x, y) = (fn x, fn y)
