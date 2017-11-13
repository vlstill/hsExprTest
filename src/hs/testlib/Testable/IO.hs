{-# LANGUAGE Safe, NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Testable.IO (
    -- * IO replacements
      IO
    , getLine
    , getChar
    , getContents
    , readIO
    , readLn
    , putChar
    , putStr
    , putStrLn
    , print
--    , readFile
--    , writeFile
--    , appendFile
    -- * running
    , runIOLines
    , runIOLines'
    ) where

import Testable.IO.Base
import Data.Functor ( Functor, fmap )
import Control.Applicative ( Applicative, pure, (<*>), liftA2 )
import Control.Monad ( Monad, (>>=) )
import Data.Monoid ( Monoid, mempty, mappend )

instance Functor IO where
    fmap = fmapIO

instance Applicative IO where
    pure = pureIO
    (<*>) = appIO

instance Monad IO where
    (>>=) = bindIO

instance Monoid a => Monoid (IO a) where
    mempty = pure mempty
    mappend = liftA2 mappend
