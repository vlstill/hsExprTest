{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}

-- (c) 2017 Vladimír Štill

module Testing.Assignment ( Typecheck (..)
                          , HintMode (..)
                          , AssignmentType (..)
                          , Assignment (..)
                          , parseAssignment
                          , WithAssignment
                          , withAssignment
                          , readAssignment
                          , doStudentOut
                          , doStudentOut'
                          , hintProceed
                          ) where

import Prelude hiding ( fail )
import Control.Arrow ( (>>>), (***) )
import Control.Monad ( foldM, when )
import Control.Monad.Fail ( MonadFail, fail )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Reader.Generalized ( ReaderT, runReaderT, GMonadReader, greader )
import Data.Char ( isSpace )
import Data.Default.Class
import Data.List ( lines, unlines )
import Data.Map ( fromList )
import Text.Read ( readEither )

import qualified Data.Map as M ( lookup )

import Types ( TypeOrdering ( Equal ) )
import Testing.Options ( Options, optAssignment, optStudent, optHint, doLog, doOut )

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
              | StudentCompileOut -- ^ ExpressionCompile + compilation output from student file
              | TypeMismatchInfo -- ^ StudentCompileOut + detailed information about type mismatch between student and solution type
              | Lint -- ^ StudentCompileOut + lint output
              | Test -- ^ Lint + whether test passed
              | TestOutput -- ^ Full output as for full test
              deriving ( Eq, Ord, Read, Show )

instance Default HintMode where
    def = StudentCompileOut

data AssignmentType = HaskellExpression
                    | HaskellType
                    deriving ( Eq, Read, Show )

-- | Parsed data of the assignment
data Assignment = Assignment { asgnExpr      :: Maybe String
                             , asgnWrapper   :: Maybe String
                             , asgnComparer  :: Maybe String
                             , asgnLimit     :: Maybe Int
                             , asgnType      :: AssignmentType
                             , asgnTypecheck :: Typecheck
                             , asgnHint      :: HintMode
                             , asgnInject    :: String
                             , asgnImports   :: [String]
                             , asgnSolution  :: String
                             , asgnStudent   :: String
                             }
                             deriving ( Eq, Read, Show )

instance Default Assignment where
    def = Assignment { asgnExpr = Nothing
                     , asgnWrapper = Nothing
                     , asgnComparer = Nothing
                     , asgnLimit = Nothing
                     , asgnType = HaskellExpression
                     , asgnTypecheck = def
                     , asgnHint = def
                     , asgnInject = ""
                     , asgnImports = []
                     , asgnSolution = ""
                     , asgnStudent = ""
                     }

type WithAssignment = ReaderT Assignment

withAssignment :: MonadIO m => Assignment -> WithAssignment m a -> m a
withAssignment asgn act = runReaderT act asgn

readAssignment :: (MonadFail m, MonadIO m, GMonadReader Options m)
               => WithAssignment m a -> m a
readAssignment act = do
    asgnData <- liftIO . readFile =<< greader optAssignment
    sol <- liftIO . readFile =<< greader optStudent
    case parseAssignment asgnData sol of
        Left msg -> fail msg
        Right asgn -> withAssignment asgn act

doStudentOut' :: (GMonadReader Options m, GMonadReader Assignment m, MonadIO m)
              => String -> m ()
doStudentOut' = doStudentOut NoOutput

doStudentOut :: (GMonadReader Options m, GMonadReader Assignment m, MonadIO m)
             => HintMode -> String -> m ()
doStudentOut hm msg = do
    proceed <- hintProceed hm
    when proceed $ doOut msg
    doLog msg

hintProceed :: (GMonadReader Options m, GMonadReader Assignment m, MonadIO m)
            => HintMode -> m Bool
hintProceed hm = do
    mode <- greader asgnHint
    hint <- greader optHint
    pure $ not hint || hm <= mode

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

parseAssignment :: String -> String -> Either String Assignment
parseAssignment asgn stud = finalize =<< foldM collapse def
    [ PE "expr" pure (\v x -> v { asgnExpr = Just x, asgnType = HaskellExpression })
    , PE "wrapper" pure (\v x -> v { asgnWrapper = Just x })
    , PE "comparer" pure (\v x -> v { asgnComparer = Just x })
    , PE "limit" (errinfo "limit must be a number" . readEither) (\v x -> v { asgnLimit = Just x })
    , PE "type" pure (\v _ -> v { asgnType = HaskellType })
    , PE "hint" (errinfo "invalid hint value" . readEither) (\v x -> v { asgnHint = x })
    , PE "import" (pure . words) (\v x -> v { asgnImports = x })
    ]
  where
    optData = getAssignmentConfigData asgn
    decoded = fromList . map ((trim *** (trim . drop 1)) . span (/= ':')) $ lines optData
    collapse :: Assignment -> ParseEntry -> Either String Assignment
    collapse a (PE key parse set) = case M.lookup key decoded of
        Nothing -> pure a
        Just v  -> parse v >>= pure . set a
    trim = dropWhile isSpace >>> reverse >>> dropWhile isSpace >>> reverse
    inj = getInject asgn
    finalize v = pure $ v { asgnSolution = asgn
                          , asgnInject = inj
                          , asgnStudent = concat [ "{-# LINE 1 \"Inject.hs\" #-}\n"
                                                 , inj
                                                 , "\n{-# LINE 1 \"Student.hs\" #-}\n"
                                                 , stud
                                                 ]
                          }

errinfo :: String -> Either String a -> Either String a
errinfo _   (Right x) = Right x
errinfo msg (Left x)  = Left (msg ++ ": " ++ x)
