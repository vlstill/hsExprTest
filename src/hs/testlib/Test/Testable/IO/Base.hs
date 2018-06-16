{-# LANGUAGE Rank2Types, LambdaCase #-}

-- (c) 2016-2018 Vladimír Štill

module Test.Testable.IO.Base (
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
    -- * functor, applicative and monad implementations
    , fmapIO
    , pureIO, appIO
    , bindIO
    ) where

-- you must hide at least the following prelude functions if you wish to use this
-- import Prelude hiding ( IO
--                      , getLine, getChar, getContents, readIO, readLn
--                      , putChar, putStr, putStrLn, print
--                      , readFile, writeFile, appendFile
--                      )

import Prelude ()
import Control.Monad ( (>>=), return )
import Prelude ( Int, String, Char, Maybe(..), Show(..), Num(..), Read(..), read
               , ($), (.)
               , reverse, map, error, head, tail, foldr )
import Control.Monad.ST ( ST, runST )
import Control.Applicative ( (<*>), (<$>), pure )
import Data.STRef ( STRef, newSTRef, readSTRef, writeSTRef, modifySTRef )

-- | 'IO' is opaque, as in Prelude, but it does not interract with outer world
-- therefore it is safe and escapable by means of 'runIOLines'
--
-- Internally, we use 'ST' to accumulate user output and line generator
-- to represent input.
data IO a = IO { unIO :: forall s. STRef s IOContext -> ST s a }

-- | Module private.
--
-- This is internal state of our 'IO' simulation
--
-- *  @generator@ function which return lines of input (numbered from 0), or 'Nothing' for EOF
-- *  @lineIx@    index of next line to read/generate
-- *  @inLine@    buffer for line which is currently being read (or 'Nothing' for EOF)
-- *  @outLine@   accumulates output (last output line is fist, lines are reversed)
data IOContext = IOContext { generator :: Int -> Maybe String
                           , lineIx :: Int
                           , inLine :: Maybe String
                           , outLines :: [String]
                           }

fmapIO :: (a -> b) -> IO a -> IO b
fmapIO f (IO io) = IO (\ctx -> f <$> io ctx)

pureIO :: a -> IO a
pureIO x = IO (\_ -> pure x)

appIO :: IO (a -> b) -> IO a -> IO b
appIO (IO f) (IO x) = IO (\ctx -> f ctx <*> x ctx)

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO (IO a) f = IO (\ctx -> a ctx >>= \x -> unIO (f x) ctx)

thenIO :: IO a -> IO b -> IO b
thenIO x y = x `bindIO` \_ -> y

mapM_IO :: (a -> IO b) -> [a] -> IO ()
mapM_IO f = foldr (thenIO . f) (pureIO ())

-- | Run virtual 'IO'
--
-- This function accepts line genrator and (virtual) 'IO' action to run
--
-- *  generator: a function which for given line number returns line of input.
--    Lines are numbered from 0, each line must not contain newline ('\n')
--    character, not even at the end.
--
-- *  action: a virtual IO action
--
-- *  return value is tuple (output of IO action, list of lines produced by
--    the 'IO' action). If no lines are produced then @[""]@ returned. Also,
--    empty line at the end is to be expected if '\n' was last character on
--    output (for example if 'putStrLn' was last action).
--
-- >>> runIOLines show $ return 0
-- (0, [""])
-- >>> runIOLines show $ getLine >>= print . (+1) . read >> getLine >>= \x -> return (read x :: Int)
-- (1, ["1",""])
runIOLines' :: (Int -> Maybe String) -> IO a -> (a, [String])
runIOLines' gen (IO io) = runST $ do
    ctx <- newSTRef (IOContext { generator = gen, lineIx = 1, inLine = gen 0, outLines = [[]] })
    r <- io ctx
    out <- outLines <$> readSTRef ctx
    return (r, reverse (map reverse out))

-- | Same as 'runIOLines'', but does not allow finite input.
runIOLines :: (Int -> String) -> IO a -> (a, [String])
runIOLines f = runIOLines' (Just . f)

-- | Read a character from the standard input device.
getChar :: IO Char
getChar = getOrErr `fmapIO` getChar'
  where
    getOrErr Nothing  = error "EOF"
    getOrErr (Just x) = x

getChar' :: IO (Maybe Char)
getChar' = IO $ \sctx -> do
    ctx <- readSTRef sctx
    case inLine ctx of
      Nothing -> return Nothing
      Just [] -> do
            let line = generator ctx (lineIx ctx)
                nctx = ctx { lineIx = lineIx ctx + 1, inLine = line }
            writeSTRef sctx nctx
            return $ Just '\n'
      Just (out:rest) -> do
            writeSTRef sctx (ctx { inLine = Just rest })
            return (Just out)

-- | Read a line from the standard input device.
getLine :: IO String
getLine = getChar `bindIO` \case
              '\n' -> pureIO []
              x -> (x:) `fmapIO` getLine

-- | The 'readIO' function is similar to 'read' except that it signals
-- parse failure to the 'IO' monad instead of terminating the program.
-- NOTE: as our IO has no exception support, this is @'pureIO' . 'read'@
readIO :: Read a => String -> IO a
readIO = pureIO . read

-- | The 'readLn' function combines 'getLine' and 'readIO'.
readLn :: Read a => IO a
readLn = getLine `bindIO` readIO

-- | The 'getContents' operation returns all user input as a single string, which
-- is read lazily as it is needed
getContents :: IO String
getContents = getChar' `bindIO` \case
                Nothing -> pureIO []
                Just x  -> (x:) `fmapIO` getContents

-- | Write a character to the standard output device.
putChar :: Char -> IO ()
putChar '\n' = IO $ \sctx -> modifySTRef sctx $ \ctx -> ctx { outLines = [] : outLines ctx }
putChar c = IO $ \sctx -> modifySTRef sctx $ \ctx -> ctx { outLines = (c : head (outLines ctx)) : tail (outLines ctx) }

-- | Write a string to the standard output device.
putStr :: String -> IO ()
putStr = mapM_IO putChar

-- | The same as putStr, but adds a newline character.
putStrLn :: String -> IO ()
putStrLn x = mapM_IO putChar x `thenIO` putChar '\n'

-- | The 'print' function outputs a value of any printable type to the
-- standard output device.
-- Printable types are those that are instances of class 'Show'; 'print'
-- converts values to strings for output using the 'show' operation and
-- adds a newline.
--
-- For example, a program to print the first 20 integers and their
-- powers of 2 could be written as:
--
-- > main = print ([(n, 2^n) | n <- [0..19]])
print :: Show a => a -> IO ()
print = putStrLn . show
