module Result where

data TestingResult
	= WontCompile
	| NotTestable
	| TypesNotEqual String String
	| DifferentValues
	| Success	
	| Timeout
		deriving Show
	
isSuccess :: TestingResult -> Bool
isSuccess Success = True
isSuccess _ = False