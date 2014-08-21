-- (c) 2014 Vladimír Štill

module Main ( main ) where

import UI ( runExpressionTester, Main )

import Network.Socket
import System.Environment ( getArgs )
import Compat.Read ( readMaybe )
import Prelude hiding ( catch )
import System.IO.Error hiding ( catch )
import System.Directory
import Control.Exception
import Data.Typeable ( typeOf )
import System.Exit

-- http://stackoverflow.com/a/8502391/1620753
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e

main :: IO ()
main = do
    [ sockaddr ] <- getArgs

    removeIfExists sockaddr
    listener <- socket AF_UNIX Stream defaultProtocol
    bind listener (SockAddrUnix sockaddr)
    listen listener 1
    loop $ do
        (sock, _) <- accept listener
  
        query <- recv sock 65536
        case readMaybe query :: Maybe Main of
            Nothing   -> send sock "INVALID\n\n"
            Just conf -> do
                (ok, msg) <- runExpressionTester conf
                send sock $ unlines
                    [ if ok then "OK" else "FAIL"
                    , ""
                    , msg
                    , ""
                    ]

        close sock
  where
    loop :: IO () -> IO ()
    loop x = x `catch` ignore >> loop x

    ignore :: SomeException -> IO ()
    ignore (SomeException e) = case (fromException (SomeException e) :: Maybe IOException) of
        Just ioe -> do putStrLn $ "WARNING: Exception: " ++ show ioe
        Nothing  -> do putStrLn $ "FATAL: Exception (" ++ show (typeOf e) ++ "): " ++ show e
                       exitFailure



