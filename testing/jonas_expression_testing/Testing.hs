{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Testing where

import Test.QuickCheck hiding (Success)
import Test.QuickCheck.Test as Test
import System.Directory
import Data.List
import Language.Haskell.Interpreter hiding (WontCompile)
import Types.Parser
import Types.Processing
import Types.Comparing()
import Files
import Result as Result
import Data.Typeable.Internal hiding (typeOf)
import Text.Printf
import Types.TypeExpression
import Config

deriving instance Typeable Test.QuickCheck.Result

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
testTypeEquality :: String -> String -> Maybe TypeExpression
testTypeEquality solution student = 
	case (parsedSolutionType, parsedStudentType) of
		(Right a, Right b) -> if a == b then Just a else Nothing
		_ -> Nothing
	where
		parsedSolutionType = parseType solution
		parsedStudentType = parseType student
	
-- | Function testLimitedExpressionValues is the main function of this module. It performs nearly all the useful work and almost all functions use it internally.
-- | This function compares given expressions within given time bounds and returns interpreter, which can be executed.
testLimitedExpressionValues :: [Maybe String] -> String -> String -> String -> Interpreter TestingResult
testLimitedExpressionValues limits expression solutionFile studentFile = do
	loadModules [solutionFile, studentFile, "InteractiveImports.Limiting", "InteractiveImports.DataTypes"]
	setImportsQ [("Prelude", Nothing), ("Student", Just "Student"), ("Solution", Just "Solution"), ("Test.QuickCheck", Nothing), ("Test.QuickCheck.Property", Just "Prop"), ("InteractiveImports.Limiting", Nothing)]
	solutionType <- typeOf ("Solution." ++ expression)
	studentType <- typeOf ("Student." ++ expression)
	case testTypeEquality solutionType studentType of 
		(Just resultType) ->
			case (getTestableArguments resultType) of
				(Just n) -> do
					let testExpression = createTestExpression expression n limits						
					action <- interpret 
						("quickCheckWithResult (stdArgs { chatty = False}) (" ++ testExpression ++ ")") 
						(as :: (IO Result))
							>>= liftIO
					return (getTestingResult action)
				(Nothing) -> return NotTestable
		(Nothing) -> return (TypesNotEqual solutionType studentType)		

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
		
getTestingResult :: Result -> TestingResult
getTestingResult Test.Success {} = Result.Success
getTestingResult Test.GaveUp {} = Result.Success
getTestingResult Test.Failure {reason = r} = if "<<timeout>>" `isInfixOf` r then Result.Timeout else Result.DifferentValues
getTestingResult Test.NoExpectedFailure {} = Result.DifferentValues
		
-- | Function run is convinience function which executes the interpreter and returns the result.
run :: Interpreter TestingResult -> IO TestingResult
run interpreter = do
	r <- runInterpreter interpreter
	case r of
		(Left error) -> return . WontCompile . show $ error
		(Right result) -> return result
