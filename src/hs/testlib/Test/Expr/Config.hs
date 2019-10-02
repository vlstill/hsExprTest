{-# LANGUAGE TypeFamilies, GADTs, ExplicitForAll, TypeOperators, DeriveDataTypeable #-}

{- | Type safe extensible test configuration. See 'getConfig' for the access of
configuration values. This is bassically a heterogeneous map type which is
indexed by special keys which carry with themselves the information about the
type of the corresponding value.
-}

{-
The keys are all singleton types to make it possible to use them both on value
and type level. The 'ConfigValue' closed type family makes sure only specified
types can be used as keys (although the exrror messages are not good).
-}

module Test.Expr.Config (
    -- * Test Configuration
    TestConfig (TestConfig),
    getConfig,
    ConfigValue,
    -- * Test Configuration Keys
    Expression (Expression), ExprName,
    Journal (Journal),
    TypeOrd (TypeOrd),
    TestPattern (TestPattern),
    DegenType (DegenType),
    -- * Low-Level Interface
    ConfigEntry (ConfigEntry)
    ) where

import Test.Journal ( JournalSink )
import Data.Typeable ( Typeable, Proxy (Proxy), typeOf )
import Data.Type.Equality ( (:~:) (Refl) )
import Unsafe.Coerce ( unsafeCoerce )

import Test.Expr.Types ( TypeOrder )
import Language.Haskell.TH ( Q, Pat, Type )

-- | A key for getting expression, value will be of type 'ExprName'
data Expression = Expression deriving (Eq, Show, Typeable)

-- | A key for getting journal, value will be of type 'JournalSink'
data Journal = Journal deriving (Eq, Show, Typeable)

-- | A key for getting TypeOrder, value will be of type 'TypeOrder'
data TypeOrd = TypeOrd deriving (Eq, Show, Typeable)

-- | A key for getting test value pattern, value will be of type @'Q' 'Pat'@
data TestPattern = TestPattern deriving (Eq, Show, Typeable)

-- | A key for getting test degeneration type, value will be of type @'Q' 'Type'@
data DegenType = DegenType deriving (Eq, Show, Typeable)

type ExprName = String

-- | A mapping from key types to corresponding types of values.
type family ConfigValue ct where
    ConfigValue Expression  = ExprName
    ConfigValue Journal     = JournalSink
    ConfigValue TypeOrd     = TypeOrder
    ConfigValue TestPattern = Q Pat
    ConfigValue DegenType   = Q Type

data ConfigEntry where
    ConfigEntry :: forall ct. (Typeable ct, Eq ct) => ct -> ConfigValue ct -> ConfigEntry

-- | Test configration, should be accessed only using the 'getConfig' function.
newtype TestConfig = TestConfig { getTestConfig :: [ConfigEntry] }

-- | Access to test configuration using test configuration keys. Suppose we
-- have a test configuration @tc@ which contains 'DegenType' and 'Expression'
-- entries (and no other entries):
--
-- >>> tc `getConfig` Expression
-- Just "foo"
-- >>> tc `getConfig` DegenType
-- Just TEqual
-- >>> tc `getConfig` Journal
-- Nothing
--
-- Please note that the type of value depends on the value (and therefore a
-- type) of the key.
getConfig :: forall ct. (Typeable ct, Eq ct) => TestConfig -> ct -> Maybe (ConfigValue ct)
getConfig (TestConfig []) _ = Nothing
getConfig (TestConfig (ConfigEntry ek ev:tcs)) k
    | Just Refl <- ek `eqT` k = Just ev
    | otherwise               = getConfig (TestConfig tcs) k
  where
    eqT :: forall a b. (Typeable a, Typeable b) => a -> b -> Maybe (a :~: b)
    eqT a b | typeOf a == typeOf b = Just $ unsafeCoerce Refl
            | otherwise            = Nothing
