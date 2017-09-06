{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-}

-- (c) 2017 Vladimír Štill

module Testing.Assignment where

import Control.Arrow ( (>>>), (***) )
import Control.Monad ( foldM )
import Data.Char ( isSpace )
import Data.Default.Class
import Data.List ( lines, unlines )
import Data.Map ( Map, fromList )
import Text.Read ( readEither )

import qualified Data.Map as M ( lookup )

import Types ( TypeOrdering ( Equal ) )

-- | Specify how typechecking should be performed
data Typecheck = NoTypecheck -- ^ do not perform any typechecking step
               | RequireTypeOrdering [TypeOrdering]
               -- ^ require that student type compared to solution type satisfies
               -- one of given ordering, for example:
               -- @RequireTypeOrdering ['Equal', 'MoreGeneral']@ means that
               -- student's type must be equal or more general then solution's
               deriving ( Eq, Read, Show )

instance Default Typecheck where
    def = RequireTypeOrdering [Equal]

-- | Determines how much information is provided before the task is submitted
data HintMode = NoOutput -- ^ nothing
              | ExpressionCompile -- ^ whether the comparison expression compiled
              | StudentCompiled -- ^ ExpressionCompile + whether the student code compiled
              | StudentCompileOut -- ^ StudentCompiled + compilation output from student file
              | HLint -- ^ StudentCompileOut + HLint output
              | Test -- ^ HLint + whether test passed
              | TestOutput -- ^ Full output as for full test
              deriving ( Eq, Read, Show )

instance Default HintMode where
    def = StudentCompileOut

-- | Parsed data of the assignment
data Assignment = Assignment { expression :: Maybe String
                             , wrapper    :: Maybe String
                             , comparer   :: Maybe String
                             , limit      :: Maybe Int
                             , typecmp    :: Bool -- ^ compare student-provided
                                             -- type with contents, do not
                                             -- compare as expressions
                             , typecheck  :: Typecheck
                             , hint       :: HintMode
                             , inject     :: String
                             , contents   :: String
                             }
                             deriving ( Eq, Read, Show )

instance Default Assignment where
    def = Assignment { expression = Nothing
                     , wrapper = Nothing
                     , comparer = Nothing
                     , limit = Nothing
                     , typecmp = False
                     , typecheck = def
                     , hint = def
                     , inject = ""
                     , contents = ""
                     }

-- | Extract YAML data from the assignment text, these are prefixed by "-- @ "
getAssignmentConfigData :: String -> String
getAssignmentConfigData = lines >>> dropWhile emptyline >>> takeWhile meta >>>
                          map (drop (length metamark)) >>> unlines
  where
    emptyline = all isSpace
    metamark = "-- @ "
    meta xs = take (length metamark) xs == metamark

-- | Get code to be injected at the beginning of the student's code
getInject :: String -> String
getInject = lines >>> dropWhile (/= beg) >>> drop 1 >>> takeWhile (/= end) >>> unlines
  where
    beg = "-- @ INJECT BEGIN"
    end = "-- @ INJECT END"

data ParseEntry = forall a. PE String (String -> Either String a) (Assignment -> a -> Assignment)

parseAssignment :: String -> Either String Assignment
parseAssignment contents = finalize =<< foldM collapse def
    [ PE "expression" pure (\v x -> v { expression = Just x })
    , PE "wrapper" pure (\v x -> v { wrapper = Just x })
    , PE "comparer" pure (\v x -> v { comparer = Just x })
    , PE "limit" (errinfo "limit must be a number" . readEither) (\v x -> v { limit = Just x })
    , PE "type" pure (\v _ -> v { typecmp = True })
    , PE "hint" (errinfo "invalid hint value" . readEither) (\v x -> v { hint = x })
    ]
  where
    optData = getAssignmentConfigData contents
    decoded = fromList . map ((trim *** (trim . drop 1)) . span (/= ':')) $ lines optData
    collapse :: Assignment -> ParseEntry -> Either String Assignment
    collapse a (PE key parse set) = case M.lookup key decoded of
        Nothing -> pure a
        Just v  -> parse v >>= pure . set a
    trim = dropWhile isSpace >>> reverse >>> dropWhile isSpace >>> reverse
    finalize v = pure $ v { contents = contents, inject = getInject contents }

errinfo :: String -> Either String a -> Either String a
errinfo _   (Right x) = Right x
errinfo msg (Left x)  = Left (msg ++ ": " ++ x)
