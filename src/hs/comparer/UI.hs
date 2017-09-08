-- (c) 2017 Vladimír Štill

module UI ( runUI ) where

import System.Exit ( exitSuccess, exitFailure )
import Data.Monoid ( Monoid(..), (<>) )
import Data.List.Split ( splitOn )
import Data.Bool ( bool )
import Testing ( runTest )
import Testing.Options
import Control.Monad ( (<=<), foldM )
import Control.Arrow ( second )
import System.IO ( hPutStrLn, stderr )

getOptions :: [String] -> Either String Options
getOptions (a:s:options) = validate . snd <=< foldM compose (False, base) $ dup
  where
    base = mempty { optAssignment = a, optStudent = s }
    dup = zip options (map Just (drop 1 options) ++ [Nothing])
    get ("--hint", _)        = next $ mempty { optHint = True }
    get ("--extra", Just fs) = eat $ mempty { optExtraFiles = splitOn "," fs }
    get ("--log", Just logf)  = eat $ mempty { optLogFile = Just logf }
    get ("--out", Just o) = eat $ mempty { optOutFile = Just o }
    get (opt, _)             = Left $ "unknown option " ++ opt ++ ", or expecting argument and found none\n" ++ usage
    validate x
      | null (optAssignment x) = Left $ "missing assignment\n" ++ usage
      | null (optStudent x)    = Left $ "missing student's solution\n" ++ usage
      | otherwise           = Right x
    next x = Right (False, x)
    eat x  = Right (True, x)
    compose (True, opts) _  = Right (False, opts)
    compose (False, opts) x = fmap (second (opts <>)) (get x)
getOptions _ = Left usage

usage :: String
usage = "usage: assignment student [--hint] [--extra file] [--log file] [--out file]"

runUI :: [String] -> IO ()
runUI opts = case getOptions opts of
                Right options -> runTest options >>= bool exitFailure exitSuccess
                Left msg      -> hPutStrLn stderr msg >> exitFailure
