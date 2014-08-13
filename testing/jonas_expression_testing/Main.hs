module Main where

import qualified Testing as Testing
import System.Environment
import Data.Maybe
import Data.List
import Result

-- | Main function, behaves differently according to the command line arguments
main :: IO ()
main = do
	args <- getArgs
	if ("-compareTypes" `elem` args)
            then compareTypes args
            else if ("-compareExpressions" `elem` args)
                then compareExpressions args
                else printHelp

-- | Function compareTypes gets student type and solution type from command line arguments and returns boolean value indicating equality of these two types
compareTypes :: [String] -> IO ()
compareTypes args = 
	case (getArg "-student" processedArgs, getArg "-solution" processedArgs) of
		(Just student, Just solution) -> invokeTypeComparison solution student
		_ -> printHelp
	where processedArgs = processArgs args

-- | Function compareTypes gets student expression and solution expression from command line arguments and returns boolean value indicating equality of these two expressions
compareExpressions :: [String] -> IO ()
compareExpressions args = 
	case (getArg "-expressionName" processedArgs, getArg "-limit" processedArgs, getArg "-student" processedArgs, getArg "-solution" processedArgs) of
		(Just expression, Nothing, Just student, Just solution) -> invokeComparison expression solution student
		(Just expression, Just limit, Just student, Just solution) -> invokeLimitedComparison (parseLimits limit) expression solution student
		_ -> printHelp
	where processedArgs = processArgs args

-- | Converts list of command line arguments to tuple representing option and value
-- | For example "-set value" is converted to [("-set", "value")]	
processArgs :: [String] -> [(String, String)]
processArgs args = filter (\(opt, value) -> isPrefixOf "-" opt && not (isPrefixOf "-" value)) (zip args (drop 1 args))

-- | Gets value of given argument from preprocessed arguments list
getArg :: String -> [(String, String)] -> Maybe String
getArg pattern [] = Nothing
getArg pattern ((option, value):xs) = if option == pattern then Just value else getArg pattern xs

-- | Prints help explaining usage of the application
printHelp :: IO ()
printHelp = do
	putStrLn "Expression tester"
	putStrLn ""
	putStrLn "Usage:"
	putStrLn "  To test type equality: programName -compareTypes -solution \"type1\" -student \"type2\""
	putStrLn "  To test expression equality: programName -compareExpressions -expressionName \"expressionName\" -solution \"expression1\" -student \"expresion2\""
	putStrLn "  To test expression equality with time limits: programName -compareExpressions -expressionName \"expressionName\" -limit \"function1;function2;...\" -solution \"expression1\" -student \"expression2\""
		
-- | Function invokeComparison runs expression comparing and prints result in the sdtout.
invokeComparison :: String -> String -> String -> IO ()
invokeComparison expression solution student = do 
	result <- Testing.compareExpressions expression solution student
	putStrLn (show $ isSuccess result)
	print result

-- | Function invokeTypeComparison runs type comparing and prints result in the sdtout.
invokeTypeComparison :: String -> String -> IO ()
invokeTypeComparison solution student = do
	let result = isJust (Testing.testTypeEquality solution student)
	putStrLn (show result)
	print (if result then Success else TypesNotEqual solution student)

-- | Function invokeLimitedComparison runs expression comparing withing given time bounds and prints result in the sdtout.
invokeLimitedComparison ::[Maybe String] -> String -> String -> String -> IO ()
invokeLimitedComparison limits expression solution student = do
	result <- Testing.compareLimitedExpressions limits expression solution student	
	putStrLn (show $ isSuccess result)
	print result

-- | Function parseLimits parses limiting expression in form "function;;;function;function;"
parseLimits :: String -> [Maybe String]
parseLimits limits = map (\xs -> if length xs == 0 then Nothing else Just xs) (split ';' limits)  

-- | Function split splits string in the given separator.
split :: Char -> String -> [String]
split = split' []

-- | Convience function
split' :: [String] -> Char -> String -> [String]
split' parts delimiter input = case (break (== delimiter) input) of
	(xs, []) -> parts ++ [xs]
	(xs, y:ys) -> split' (parts ++ [xs]) delimiter ys
