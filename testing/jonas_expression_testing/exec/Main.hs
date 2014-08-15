module Main ( main ) where

import System.Environment ( getArgs )
import System.IO ( stderr )
import UI ( runUI )

main :: IO ()
main = getArgs >>= runUI
