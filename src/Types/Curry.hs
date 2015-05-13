{-# LANGUAGE TypeFamilies #-}

-- (c) 2014 Vladimír Štill

-- | Generic curry for 2 to 7 tuples
module Types.Curry ( GCurry( guncurry, gcurry ), Curry ) where

class GCurry a where
    type Curry a r :: *
    gcurry   :: (a -> r) -> Curry a r
    guncurry :: Curry a r -> a -> r

instance GCurry (a, b) where
    type Curry (a, b) r = a -> b -> r
    gcurry = curry
    guncurry = uncurry

instance GCurry (a, b, c) where
    type Curry (a, b, c) r = a -> b -> c -> r
    gcurry fun a b c = fun (a, b, c)
    guncurry fun (a, b, c) = fun a b c

instance GCurry (a, b, c, d) where
    type Curry (a, b, c, d) r = a -> b -> c -> d -> r
    gcurry fun a b c d = fun (a, b, c, d)
    guncurry fun (a, b, c, d) = fun a b c d

instance GCurry (a, b, c, d, e) where
    type Curry (a, b, c, d, e) r = a -> b -> c -> d -> e -> r
    gcurry fun a b c d e = fun (a, b, c, d, e)
    guncurry fun (a, b, c, d, e) = fun a b c d e

instance GCurry (a, b, c, d, e, f) where
    type Curry (a, b, c, d, e, f) r = a -> b -> c -> d -> e -> f -> r
    gcurry fun a b c d e f = fun (a, b, c, d, e, f)
    guncurry fun (a, b, c, d, e, f) = fun a b c d e f

instance GCurry (a, b, c, d, e, f, g) where
    type Curry (a, b, c, d, e, f, g) r = a -> b -> c -> d -> e -> f -> g -> r
    gcurry fun a b c d e f g = fun (a, b, c, d, e, f, g)
    guncurry fun (a, b, c, d, e, f, g) = fun a b c d e f g

