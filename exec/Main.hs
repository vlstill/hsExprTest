-- (c) 2014 Vladimír Štill

module Main ( main ) where

import System.Environment ( getArgs )
import UI ( runUI )

main :: IO ()
main = getArgs >>= runUI
