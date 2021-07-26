module Test.QuickCheck.GenConvertible where

-- (c) 2018 Vladimír Štill

import Prelude ( Int, map, ($), (<$>), pure, foldl, foldr, zipWith, Maybe (..) )
import Language.Haskell.TH ( Exp (..), Type (..), Dec (..), Pat (..), Q
                           , mkName, newName, tupleTypeName
                           , Overlap (..), Clause (..), Body (..) )
import Control.Monad ( replicateM )
import Test.Expr.Internal.Compat

-- instance {-# OVERLAPS #-} (Convertible a a', Convertible b b') => Convertible (Fun a' b) (a -> b') where
--     convert' (Fun _ f) x = convert' (f (convert' x))

-- | For each n >= 0 build an
-- @instance OVERLAPS … => Convertible (Fun (a1', a2', …) b) (a1 -> a2 -> … -> b')@
convertibleN :: Int -> Q Dec
convertibleN n = do
    let convertible = mkName "Convertible"
        convert = mkName "convert'"
        fun = mkName "Fun"
    as <- map VarT <$> replicateM n (newName "a")
    a's <- map VarT <$> replicateM n (newName "a'")
    f <- newName "f"
    b <- VarT <$> newName "b"
    b' <- VarT <$> newName "b'"
    xs <- replicateM n (newName "x")

    let ntup = ConT $ tupleTypeName n
        intuple'_t = foldl AppT ntup a's -- original tuple of input types

        in_t = ConT fun `AppT` intuple'_t `AppT` b -- (Fun (a1', a2', …) b)
        out_t = foldr (\inp ret -> (ArrowT `AppT` inp) `AppT` ret) b' as -- a1 -> (a2 -> (… -> b'))

        -- (Convertible a1 a1', Convertible a2 a2', …, Convertible b b') =>
        cxt = zipWith (\x y -> (ConT convertible `AppT` x) `AppT` y) (b:as) (b':a's)
        -- Convertible (Fun (a1', a2', …) b) (a1 -> a2 -> … -> b')
        head = (ConT convertible `AppT` in_t) `AppT` out_t

        fun_p = ConP fun [WildP, VarP f] -- (Pat _ f)

        -- (convert x1, convert x2, …)
        argtuple = TupE $ map (\x -> wrapTupElemE (VarE convert `AppE` VarE x)) xs
        -- convert (f (convert x1, convert x2, …))
        body = VarE convert `AppE` (VarE f `AppE` argtuple)

        clause = Clause (fun_p : map VarP xs) (NormalB body) []

    pure $ InstanceD (Just Overlaps) cxt head [FunD convert [clause]]
