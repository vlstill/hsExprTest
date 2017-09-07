{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             DataKinds,
             TypeFamilies,
             UndecidableInstances,
             ScopedTypeVariables
             #-}

-- | Generalizes 'Control.Monad.Reader.MonadReader' to stacks of readers
module Control.Monad.Reader.Generalized ( GMonadReader
                                        , greader
                                        -- * re-exports
                                        , ReaderT
                                        , runReaderT
                                        ) where

import Control.Monad.Reader ( ReaderT, MonadReader, reader, local, runReaderT )
import Control.Monad.Trans.Class ( MonadTrans, lift )
import Data.Proxy ( Proxy ( Proxy ) )

-- | Generalized 'MonadReader' class which reads from the fist reader in the
-- monadic stack which can be used (based on type of value being read).
-- You should not write instances for this (except for possibly deriving them
-- using GeneralizedNewtypeDeriving), instances are derived from 'MonadReader'
-- instances automatically.
-- Note that 'Control.Monad.Reader.local' is missing as it is not easy to push
-- it down the level of monadic stack.
class GMonadReader r m where
    greader :: (r -> a) -> m a

    gask :: m r
    gask = greader id

instance (IsTopLevelReader r m ~ flag, GMonadReader' flag r m) => GMonadReader r m where
    greader = greader' (Proxy :: Proxy flag)

class GMonadReader' (flag :: Bool) r m where
    greader' :: Proxy flag -> (r -> a) -> m a

instance (MonadTrans t, Monad m, GMonadReader r m) => GMonadReader' 'False r (t m) where
    greader' _ = lift . greader

instance Monad m => GMonadReader' 'True r (ReaderT r m) where
    greader' _ = reader

type family (IsTopLevelReader r m) :: Bool where
    IsTopLevelReader r (ReaderT r m') = 'True
    IsTopLevelReader r m              = 'False
