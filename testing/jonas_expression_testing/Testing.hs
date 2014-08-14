{-# LANGUAGE StandaloneDeriving
           , DeriveDataTypeable
           #-}

module Testing where

import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Test as QCT
import System.Directory
import Data.List
import Language.Haskell.Interpreter hiding ( WontCompile )
import qualified Language.Haskell.Interpreter as I
import Language.Haskell.Interpreter.Unsafe ( unsafeRunInterpreterWithArgs )
import Types.Parser
import Types.Processing
import Types.Comparing ( compareTypes )
import Files
import Result as Result
import Data.Typeable.Internal hiding (typeOf)
import Text.Printf
import Types.TypeExpression
import Config

deriving instance Typeable QC.Result

-- | Function compareExpressions compares two expressions using QuickCheck and return result as IO String
compareExpressions :: String -> String -> String -> IO TestingResult
compareExpressions = testFiles testExpressionValues

-- | Function compareLimitedExpressions compares two expressions using QuickCheck within given time bounds and return result as IO String.
-- | First argument is list of expressions representing functions which calculate time bound given corresponding argument.
compareLimitedExpressions :: [Maybe String] -> String -> String -> String -> IO TestingResult
compareLimitedExpressions limits = testFiles (testLimitedExpressionValues limits)

-- | Function testFiles creates two testing modules containing student and solution expressions and runs given interpreter on those modules.
testFiles :: (String -> String -> String -> Interpreter TestingResult) -> String -> String -> String -> IO (TestingResult)
testFiles interpreter expression solution student = do
    solutionFile <- createSolutionFile solution
    studentFile <- createStudentFile student
    result <- run $ interpreter expression solutionFile studentFile
    removeFile solutionFile
    removeFile studentFile
    return result

-- | Convinience function
testExpressionValues :: String -> String -> String -> Interpreter TestingResult
testExpressionValues = testLimitedExpressionValues []

-- | Function testTypeEquality compares two given type expressions.
testTypeEquality :: String -> String -> TypingResult
testTypeEquality solution student =
    case (parsedSolutionType, parsedStudentType) of
        (Right a, Right b) -> a `compareTypes` b
        (_, Left _) -> CannotParse student
        (Left _, _) -> CannotParse solution
    where
        parsedSolutionType = parseType solution
        parsedStudentType = parseType student

-- | Function testLimitedExpressionValues is the main function of this module. It performs nearly all the useful work and almost all functions use it internally.
-- | This function compares given expressions within given time bounds and returns interpreter, which can be executed.
testLimitedExpressionValues :: [Maybe String] -> String -> String -> String -> Interpreter TestingResult
testLimitedExpressionValues limits expression solutionFile studentFile = do
    set [ installedModulesInScope := False -- security reasons, avoid accessing internals
        ]
    loadModules [ solutionFile, studentFile
                , "InteractiveImports.Limiting", "InteractiveImports.DataTypes"
                ]
    setImportsQ [ ("Prelude", Nothing)
                , ("Student", Just "Student")
                , ("Solution", Just "Solution")
                , ("Test.QuickCheck", Nothing)
                , ("Test.QuickCheck.Property", Just "Prop")
                , ("InteractiveImports.Limiting", Nothing)
                ]
    solutionType <- typeOf ("Solution." ++ expression)
    studentType <- typeOf ("Student." ++ expression)
    case testTypeEquality solutionType studentType of
        TypesEqual resultType ->
            case (getTestableArguments resultType) of
                Just n -> do
                    let testExpression = createTestExpression expression n limits
                    action <- interpret
                        ("quickCheckWithResult (stdArgs { chatty = False}) (" ++ testExpression ++ ")")
                        (as :: (IO QCT.Result))
                            >>= liftIO
                    return (getTestingResult action)
                Nothing -> return NotTestable
        r -> return (TypesNotEqual r)

-- | Function createTestExpressions creates one string test expression from two function expressions.
-- | It creates lambda expression by prepending correct number of arguments and comparing result of given functions when applied to those arguments.
-- | Generated lambda expression also contains time limitation computed by function createLimits
createTestExpression :: String -> Int -> [Maybe String] -> String
createTestExpression expression argumentsCount limits =
        printf ("\\%s -> within %s (Solution.%s %s == Student.%s %s)") argumentsExpression limitExpression expression argumentsExpression expression argumentsExpression
            where
                arguments = (map (('x':) . show) [1..argumentsCount])
                limitedArguments = if (null limits) then [] else zip arguments (limits ++ repeat Nothing)
                limitExpression = createLimits limitedArguments
                argumentsExpression = concat $ intersperse " " arguments

-- | Function createLimits creates limiting expression by applying complexity functions to respective aruguments and taking minimum of results.
-- | It multiplies result with given constant and adds another constant to the result, to convert complexity bounds to time bounds and to represent Big-O notation correctly.
createLimits :: [(String, Maybe String)] -> String
createLimits [] = show Config.defaultLimit
createLimits [(_, Nothing)] = show Config.defaultLimit
createLimits limits = "(" ++ show Config.additionConstant ++ " + " ++ show Config.multiplicationConstant ++" * (minimum [" ++ limitsExpression ++ "]))"
    where
        process (_, Nothing) = ""
        process (argument, Just limitExpression) = limitExpression ++ " $ fromIntegral(parameter " ++ argument ++ ")"
        limitsExpression = concat (intersperse ", "  (map process limits))

getTestingResult :: QCT.Result -> TestingResult
getTestingResult QCT.Success {} = Result.Success
getTestingResult QCT.GaveUp {} = Result.Success
getTestingResult QCT.Failure { QCT.reason = r, QCT.output = o }
    = if "<<timeout>>" `isInfixOf` r then Result.Timeout else Result.DifferentValues o
getTestingResult QCT.NoExpectedFailure { QCT.output = o } = Result.DifferentValues o

-- | Function run is convinience function which executes the interpreter and returns the result.
run :: Interpreter TestingResult -> IO TestingResult
run interpreter = do
    r <- unsafeRunInterpreterWithArgs pkgs interpreter
    case r of
        (Left error) -> case error of
            I.UnknownError str -> ce $ "Unknown error: " ++ str
            I.NotAllowed  str  -> ce $ "Not allowed: " ++ str
            I.GhcException str -> ce $ "GHC exception: " ++ str
            I.WontCompile list -> ce $ "Compilation error:\n" ++ unlines (map I.errMsg list)
        (Right result) -> return result
  where
    ce = return . WontCompile
    pkgs = map ("-package=" ++) [ "random", "tf-random", "QuickCheck" ]

