module Config where

-- | Value representing constant, by which is multiplied every computed time limit
multiplicationConstant :: Int
multiplicationConstant = 10000

-- | Value representing constant, which is added to every computed time limit
additionConstant :: Int
additionConstant = 100000

-- | Value representing constant, which is used as time limit when no limiting function is specified
defaultLimit :: Int
defaultLimit = 100000