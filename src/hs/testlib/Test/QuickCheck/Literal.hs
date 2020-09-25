{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeFamilies #-}
-- | Defines newtypes for strings and integers that can be used to generate
-- constants. The concrete value of the constant is encoded in the type, e.g.,
-- 'StringConst "hi!"' will always generate string "hi!" using arbitrary.
-- It is expected the values will be coerced to the underlying data type before
-- use using 'coerce' or 'fromConst'.
-- Converting to 'IntConst' or 'StrConst' using 'Coercible' is undefined
-- behaviour unless the actual value matches the value in the type.

module Test.QuickCheck.Literal
    ( IntConst ( IntConst ),
      StrConst ( StrConst ),
      StringConst,
      FromConst ( fromConst, ConstRep )
    ) where

import Data.Default.Class ( Default, def )
import Data.Proxy
import Data.Kind ( Type )
import Data.String ( IsString, fromString )
import GHC.TypeLits ( Nat, KnownNat, natVal, Symbol, KnownSymbol, symbolVal )
import Test.QuickCheck.Arbitrary ( Arbitrary, arbitrary, shrink )

-- | The data constructor should not be used, can be only created by 'arbitrary' or 'def'
newtype IntConst (n :: Nat) (i :: Type) = IntConst i

instance (KnownNat n, Integral i, Show i) => Show (IntConst n i) where
    show (IntConst i) = show i

-- | '(==)' always returns True, 'IntConst' is a singleton type.
instance (KnownNat n, Integral i) => Eq (IntConst n i) where
    _ == _ = True

instance forall n i. (KnownNat n, Integral i) => Default (IntConst n i) where
    def = IntConst (fromInteger (natVal (Proxy :: Proxy n)))

-- | Generates the single value encoded in the type, does not shrink.
instance (KnownNat n, Integral i) => Arbitrary ( IntConst n i ) where
    arbitrary = pure def
    shrink _ = [] -- can't do anything


-- | The data constructor should not be used, can be only created by 'arbitrary' or 'def'
newtype StrConst (sym :: Symbol) (str :: Type) = StrConst str

type StringConst (sym :: Symbol) = StrConst sym String

instance (KnownSymbol sym, IsString str, Show str) => Show (StrConst sym str) where
    show (StrConst x) = show x

-- | '(==)' always returns True, 'IntConst' is a singleton type.
instance (KnownSymbol sym, IsString str) => Eq (StrConst sym str) where
    _ == _ = True

instance forall sym str. (KnownSymbol sym, IsString str) => Default (StrConst sym str) where
    def = StrConst (fromString (symbolVal (Proxy :: Proxy sym)))

-- | Generates the single value encoded in the type, does not shrink.
instance (KnownSymbol sym, IsString str) => Arbitrary (StrConst sym str) where
    arbitrary = pure def
    shrink _ = [] -- can't do anything

class FromConst c where
    type ConstRep c :: Type
    fromConst :: c -> ConstRep c

instance forall n i. (KnownNat n, Integral i) => FromConst (IntConst n i) where
    type ConstRep (IntConst n i) = i
    fromConst (IntConst x) = x

instance forall sym str. (KnownSymbol sym, IsString str) => FromConst (StrConst sym str) where
    type ConstRep (StrConst sym str) = str
    fromConst (StrConst x) = x
