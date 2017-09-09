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
import Control.Applicative ( Applicative, pure, (<*>) )
import Control.Monad ( Monad, (>>=) )

instance Functor IO where
    fmap = fmapIO

instance Applicative IO where
    pure = pureIO
    (<*>) = appIO

instance Monad IO where
    (>>=) = bindIO
