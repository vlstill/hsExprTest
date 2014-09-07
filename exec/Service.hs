{-# LANGUAGE NamedFieldPuns #-}
-- (c) 2014 Vladimír Štill

module Main ( main ) where

import UI ( runExpressionTester, Main )

import Prelude hiding ( catch )

import Control.Exception
import Control.Arrow

import System.Environment ( getArgs )
import System.IO.Error hiding ( catch )
import System.Directory
import System.Exit
import System.IO
import System.Posix.Files

import Network.Socket

import Compat.Read ( readEither )
import Data.Char
import Data.Typeable ( typeOf )
import Data.Either
import Data.Monoid
import Data.Bits ( (.|.) )

data Query
    = Query { transactId :: Integer
            , questionId :: Integer
            , content    :: String
            }
    deriving ( Eq, Show, Read )

-- http://stackoverflow.com/a/8502391/1620753
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e

main :: IO ()
main = getArgs >>= \args -> case args of
  [ sockaddr ] -> runSocket sockaddr
  []           -> runSocket "/var/lib/checker/socket"


runSocket :: FilePath -> IO ()
runSocket sockaddr = do
    removeIfExists sockaddr
    listener <- socket AF_UNIX Stream defaultProtocol
    bind listener (SockAddrUnix sockaddr)
    setFileMode sockaddr $ foldr1 (.|.) [ ownerReadMode, ownerWriteMode
                                        , groupReadMode, groupWriteMode
                                        , otherReadMode, otherWriteMode ]
    listen listener 1
    loop $ do
        (sock, _) <- accept listener
  
        query <- recv sock 65536

        hPutStrLn stderr $ "Received: '" ++ query ++ "'"
        case parseQ query of
            Left msg -> send sock ("INVALID: " ++ msg ++ "\n\n") >> return ()
            Right query -> do
                hPutStrLn stderr . ("Query: " ++ ) . show $ query
                runQuery query sock
        close sock
  where
    loop :: IO () -> IO ()
    loop x = x `catch` ignore >> loop x

    ignore :: SomeException -> IO ()
    ignore (SomeException e) = case (fromException (SomeException e) :: Maybe IOException) of
        Just ioe -> do putStrLn $ "WARNING: Exception: " ++ show ioe
        Nothing  -> do putStrLn $ "FATAL: Exception (" ++ show (typeOf e) ++ "): " ++ show e
                       exitFailure

runQuery :: Query -> Socket -> IO ()
runQuery (Query { transactId, questionId, content }) sock = do
    send sock $ "I" ++ show transactId ++ "P0C"
    return ()
{-
            do
                (ok, msg) <- runExpressionTester conf
                send sock $ unlines
                    [ if ok then "OK" else "FAIL"
                    , ""
                    , msg
                    , ""
                    ]

-}

both :: Monoid e => (Either e a, Either e b) -> Either e (a, b)
both (Right x, Right y) = Right (x, y)
both (Left x, Left y)   = Left (x `mappend` y)
both (Left x, _)        = Left x
both (_, Left y)        = Left y

-- FORMAT: "I<xid>Q<id>S<len><odp>";
parseQ :: String -> Either String Query
parseQ ('I':qs) = span isDigit >>> readEither *** parseQuestion >>> both >>> fmap toQuery $ qs
  where
    parseQuestion :: String -> Either String (Integer, String)
    parseQuestion ('Q':qs) = span isDigit >>> readEither *** parseContent >>> both $ qs
    parseQuestion _        = Left "Expected 'Q'. "

    parseContent :: String -> Either String String
    parseContent ('S':qs) = Right qs
    parseContent _        = Left "Expected 'S'. "

    parseContentLen (Right (len, cont)) =
        if length cont == len then Right cont
            else Left $ "Wrong content length, expected " ++ show len ++ " got " ++ show (length cont) ++ ". "
    parseContentLen (Left msg) = Left msg

    toQuery (transactId, (questionId, content)) = Query { transactId, questionId, content }
parseQ _ = Left "Expected 'I' at the beginning. "

