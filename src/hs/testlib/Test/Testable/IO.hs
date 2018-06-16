{-# LANGUAGE Safe, NoImplicitPrelude, CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- (c) 2016-2018 Vladimír Štill

module Test.Testable.IO (
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

import Test.Testable.IO.Base
import Data.Functor ( Functor, fmap )
import Control.Applicative ( Applicative, pure, (<*>), liftA2 )
import Control.Monad ( Monad, (>>=) )
import Data.Monoid ( Monoid, mempty, mappend )
import Data.Semigroup ( Semigroup, (<>) )

instance Functor IO where
    fmap = fmapIO

instance Applicative IO where
    pure = pureIO
    (<*>) = appIO

instance Monad IO where
    (>>=) = bindIO

instance Semigroup a => Semigroup (IO a) where
    (<>) = liftA2 (<>)

instance (Semigroup a, Monoid a) => Monoid (IO a) where
    mempty = pure mempty
    mappend = (<>)
