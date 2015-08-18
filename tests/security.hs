{-# LANGUAGE TupleSections #-}
module Main ( main ) where

import Types.Curry
import Harness

main = runTests $ flip (zipWith guncurry) (repeat ("f = map", "f", CompileError ignored))
    [ ("import qualified Solution\nf = Solution.f",,,)
    , ("import System.IO.Unsafe\nf = unsafePerformIO (return map)",,,)
    -- implicit import
    , ("f = Solution.f",,,)
    , ("f = System.IO.Unsafe.unsafePerformIO (returm map)",,,)
    ]
