{-# LANGUAGE NamedFieldPuns, RecordWildCards, MultiWayIf #-}
-- (c) 2014-2015 Vladimír Štill

module Main ( main ) where

import Testing
import Result
import PrettyPrint
import Types

import Control.Exception
import Control.Arrow
import Control.Applicative
import Control.Monad

import System.Environment ( getArgs )
import System.IO.Error
import System.Directory
import System.Exit
import System.IO
import System.Posix.Files
import System.FilePath
import System.Clock

import Network.Socket

import Text.Read ( readEither )
import Data.Char
import Data.Typeable ( typeOf )
import Data.Maybe
import Data.Monoid
import Data.Bits ( (.|.) )
import Data.List

data Query
    = Query { transactId :: Integer
            , questionId :: Integer
            , content    :: String
            , hintOnly   :: Bool
            }
    deriving ( Eq, Show, Read )

-- http://stackoverflow.com/a/8502391/1620753
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e

defaultsock, defaultqdir :: FilePath
defaultsock = "/var/lib/checker/socket"
defaultqdir = "/var/lib/checker/qdir"

deflimit :: Int
deflimit = 2000 * 1000 -- 1 seconds

doLog :: String -> IO ()
doLog = hPutStrLn stderr

main :: IO ()
main = getArgs >>= \args -> case args of
  [ sockaddr, qdir ] -> runSocket sockaddr qdir
  [ sockaddr ]       -> runSocket sockaddr defaultqdir
  []                 -> runSocket defaultsock defaultqdir
  _                  -> error "usage: hsExprTestService [ sockaddr [ qdir ] ]"


runSocket :: FilePath -> FilePath -> IO ()
runSocket sockaddr0 qdir = do
    pwd <- getCurrentDirectory
    let sockaddr = if isAbsolute sockaddr0 then sockaddr0 else pwd </> sockaddr0
    setCurrentDirectory qdir
    removeIfExists sockaddr
    listener <- socket AF_UNIX Stream defaultProtocol
    bind listener (SockAddrUnix sockaddr)
    setFileMode sockaddr $ foldr1 (.|.) [ ownerReadMode, ownerWriteMode
                                        , groupReadMode, groupWriteMode
                                        , otherReadMode, otherWriteMode ]
    listen listener 1
    loop $ do
        doLog "accepting socket..."
        bracket (fst <$> accept listener) close $ \sock -> do
            doLog $ "accepted fd=" ++ show ((\(MkSocket fd _ _ _ _) -> fd) sock)

            doLog "receiving query..."
            query <- recv sock 65536
            start <- getTime Monotonic
            doLog "done"

            hPutStrLn stderr $ "Received: '" ++ query ++ "'"
            case parseQ query of
                Left msg -> void $ send sock ("INVALID: " ++ msg ++ "\n\n")
                Right query -> do
                    doLog . ("Running query: " ++ ) . show $ query
                    runQuery query sock
                    doLog "query done"
            end <- getTime Monotonic
            doLog $ "took " ++ show (diffTime (10^3) end start) ++ " milliseconds"
  where
    loop :: IO () -> IO ()
    loop x = x `catch` ignore >> loop x

    ignore :: SomeException -> IO ()
    ignore (SomeException e) = case (fromException (SomeException e) :: Maybe IOException) of
        Just ioe -> doLog $ "WARNING: Exception: " ++ show ioe
        Nothing  -> do doLog $ "FATAL: Exception (" ++ show (typeOf e) ++ "): " ++ show e
                       exitFailure

    diffTime :: Integer -> TimeSpec -> TimeSpec -> Integer
    diffTime prec a b = ndiff + sdiff
      where
        ndiff = (`div` (10^9 `div` prec)) . fromIntegral $ nsec a - nsec b
        sdiff = (* prec) . fromIntegral $ sec a - sec b

runQuery :: Query -> Socket -> IO ()
runQuery (Query {..}) sock = do
    let qfile = show questionId `addExtension` "q.hs"
    let err str = send sock $ concat [ "I", show transactId, "Pnok", "C", str ]
    fe <- doesFileExist qfile
    if not fe then void (err "FATAL: Question does not exits") else do
        question <- decodeQ content <$> readFile qfile
        case question of
            Left emsg -> void $ err ("FATAL: Invalid question: " ++ emsg)
            Right q   -> do
                doLog "running expressionTester"
                doLog $ unlines [ "Testing:"
                                , "    Student:"
                                , unlines (map ("    " ++) (lines (student q)))
                                , ""
                                , "    Solution:"
                                , unlines (map ("    " ++) (lines (solution q)))
                                , ""
                                ]
                (ok, msg) <- fmap (isSuccess &&& (pp >>> map fixTicks)) $ runTest q
                doLog "done"
                let reply = concat [ "I", show transactId
                                   , "P", if ok then "ok" else "nok"
                                   , "C", if hintOnly then "" else msg ]
                doLog $ "replying on socket, reply = " ++ reply
                when hintOnly $ doLog $ "hint hidden message: " ++ msg
                send sock reply
                doLog "reply sent"
                return ()
  where
    decodeQ :: String -> String -> Either String Test
    decodeQ student q = case span ("-- @ " `isPrefixOf`) (lines q) of
        ([], _)           -> Left "Missing instructions"
        (instr, solution) -> fromInstr student (unlines solution) (map (drop 5) instr)

    fixTicks '‘' = '`'
    fixTicks '’' = '\''
    fixTicks x   = x

    fromInstr student0 solution instrs0 =
        case (lookup "type" instrs, lookup "expr" instrs) of
            (Just _, Nothing) -> addmode CompareTypes { student = student0, solution }
            (Nothing, Just expressionName) -> do
                limit <- fmap Just . maybe (Right deflimit) readEither $ lookup "limit" instrs
                student <- if isJust (lookup "inject" instrs)
                    then do
                        let toInject = unlines . takeWhile (/= "-- @ INJECT END")
                                               . dropWhile (/= "-- @ INJECT BEGIN")
                                               . lines $ solution
                        Right $ unlines [ "{-# LINE 1 \"Inject.hs\" #-}"
                                        , toInject
                                        , ""
                                        , "{-# LINE 1 \"Student.hs\" #-}"
                                        , student0
                                        ]
                    else Right student0
                addmode CompareExpressions { student, solution, expressionName, limit }
            _ -> Left "Invalid instructions (expected either '-- @ type' or '-- @ expr: NAME')"
      where
        instrs = map (span (/= ':') >>> second (drop 2)) instrs0
        addmode :: Test -> Either String Test
        addmode test = do
            let modename = if hintOnly then "hintmode" else "mode"
                defmode = if | hintOnly && isTypecheck test -> JustCompile
                             | hintOnly  -> CompileAndTypecheck
                             | otherwise -> FullComparison
            compareMode <- maybe (Right defmode) parseCompareMode $ lookup modename instrs
            typecheckMode <- maybe (Right $ RequireTypeOrdering [ Equal ]) parseTypecheck $ lookup "typecheck" instrs
            return $ test { compareMode, typecheckMode }
        parseCompareMode :: String -> Either String CompareMode
        parseCompareMode str
            | str == "compile"   = Right JustCompile
            | str == "typecheck" = Right CompileAndTypecheck
            | str == "full"      = Right FullComparison
            | otherwise           = Left "Expected one of 'compile', 'typecheck', 'full'"

        isTypecheck :: Test -> Bool
        isTypecheck CompareTypes {} = True
        isTypecheck _ = False

        parseTypecheck :: String -> Either String Typecheck
        parseTypecheck = words >>> mapM (`lookupE` cms) >>> fmap RequireTypeOrdering
        cms = [ ("=", Equal), ("<", LessGeneral), (">", MoreGeneral)
              , ("u", Unifiable), ("n", NotUnifiable) ]

        lookupE x m = maybe (Left $ "Could not parse " ++ show x) Right $ x `lookup` m

both :: Monoid e => (Either e a, Either e b) -> Either e (a, b)
both (Right x, Right y) = Right (x, y)
both (Left x, Left y)   = Left (x `mappend` y)
both (Left x, _)        = Left x
both (_, Left y)        = Left y

-- FORMAT: "I<xid>Q<id>S<solution>" or "HI<xid>Q<id>S<solution>";
parseQ :: String -> Either String Query
parseQ query = case query of
                    'H':'I':qs -> addHint True <$> parseQ' qs
                    'I':qs     -> addHint False <$> parseQ' qs
                    _          -> Left "Expected 'I' or 'HI' at the beginning. "
  where
    parseQ' = span isDigit >>> readEither *** parseQuestion >>> both >>> fmap toQuery
    addHint h x = x { hintOnly = h }

    parseQuestion :: String -> Either String (Integer, String)
    parseQuestion ('Q':qs) = ($ qs) $
            span isDigit >>>
            (readEither >>> left (const "Expected number after 'Q'")) *** parseContent >>>
            both
    parseQuestion _        = Left "Expected 'Q'. "

    parseContent :: String -> Either String String
    parseContent ('S':qs) = Right qs
    parseContent _        = Left "Expected 'S'. "

    toQuery (transactId, (questionId, content)) = Query { transactId, questionId, content }

