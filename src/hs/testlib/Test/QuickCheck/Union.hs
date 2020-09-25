{-# LANGUAGE DataKinds,
             ScopedTypeVariables, 
             TypeFamilies,
             MultiParamTypeClasses,
             FlexibleInstances,
             UndecidableInstances,
             ConstraintKinds,
             PolyKinds,
             FlexibleContexts,
             TypeOperators,
             UnicodeSyntax
             #-}

module Test.QuickCheck.Union
    (
      Union ( Union )
    ) where

import Data.Coerce ( Coercible, coerce )
import Data.Proxy
import Data.Kind ( Type, Constraint )
import Test.QuickCheck ( Arbitrary, Gen, arbitrary, shrink, frequency )
import GHC.TypeLits

newtype Union (target :: Type) (alternatives :: [Type]) = Union target

class AllConstraint (cs :: Type -> Constraint) (alt :: [Type]) where
instance ∀ (cs :: Type -> Constraint). AllConstraint cs '[]
instance ∀ (cs :: Type -> Constraint) (a :: Type) (as :: [Type]).
         (cs a, AllConstraint cs as) => AllConstraint cs (a ': as)

class TypeListLenght (xs :: [κ]) where
    type ListLenght (xs :: [κ]) :: Nat

typeListLength :: ∀ κ (xs :: [κ]) proxy.
                  (TypeListLenght xs, KnownNat (ListLenght xs)) => proxy xs -> Int
typeListLength _ = fromInteger (natVal (Proxy :: Proxy (ListLenght xs)))

instance TypeListLenght '[] where
    type ListLenght '[] = 0

instance ∀ κ (x :: κ) (xs :: [κ]).
         TypeListLenght xs => TypeListLenght (x ': xs) where
    type ListLenght (x ': xs) = 1 + ListLenght xs

instance (Show target, AllConstraint (Coercible target) alternatives)
          => Show (Union target alternatives) where
    show (Union x) = show x

instance {-# OVERLAPPING #-}
        ∀ target alt. (Coercible target alt, Arbitrary alt)
        => Arbitrary (Union target '[alt]) where
    arbitrary = coerce <$> (arbitrary :: Gen alt)
    shrink x = coerce <$> shrink (coerce x :: alt)

instance {-# OVERLAPPING #-}
         ∀ target alt (as :: [Type]).
          (TypeListLenght as, KnownNat (ListLenght as), 1 <= ListLenght as,
            Coercible target alt, Arbitrary alt,
            Arbitrary (Union target as))
          => Arbitrary (Union target (alt ': as)) where
    arbitrary = frequency [(1, coerce <$> (arbitrary :: Gen alt)),
                           (typeListLength (Proxy :: Proxy as), coerce <$> (arbitrary :: Gen (Union target as)))]
    shrink _ = []
