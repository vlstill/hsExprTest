{-# LANGUAGE CPP #-}

module Control.Monad.Reader.Fail () where

#ifdef NO_BASE_FAIL
import Prelude ()
import Control.Monad.Fail ( MonadFail ( fail ) )
import Control.Monad.Reader ( ReaderT )
import Control.Monad.Trans.Class ( lift )

instance MonadFail m => MonadFail (ReaderT r m) where
    fail msg = lift (fail msg)
#endif
