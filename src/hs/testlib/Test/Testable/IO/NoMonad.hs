{-# LANGUAGE Safe, NoImplicitPrelude #-}

-- (c) 2016-2018 Vladimír Štill

module Test.Testable.IO.NoMonad (
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
    -- * functor, applicative and monad replacements
    , fmap, (<$>), (<*>), pure
    , (>>=), (>>), return
    ) where

import Test.Testable.IO.Base

fmap, (<$>) :: (a -> b) -> IO a -> IO b
fmap = fmapIO
(<$>) = fmapIO
infixl 4 <$>

pure :: a -> IO a
pure = pureIO

(<*>) :: IO (a -> b) -> IO a -> IO b
(<*>) = appIO
infixl 4 <*>

return :: a -> IO a
return = pureIO

(>>=) :: IO a -> (a -> IO b) -> IO b
(>>=) = bindIO
infixl 1 >>=

(>>) :: IO a -> IO b -> IO b
x >> y = x >>= const y
infixl 1 >>

