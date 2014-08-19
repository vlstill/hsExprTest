{-# LANGUAGE Safe #-}

-- (c) 2012 Martin Jonáš

module Testing.Limiting (Parametrizable(..)) where

-- | Class Parametrizable represents data types for which there is a function, which returns number representing size of the given value.
class Parametrizable a where
    parameter :: a -> Int

instance Parametrizable Int where
    parameter = abs

instance Parametrizable Integer where
    parameter = abs . fromInteger

instance Parametrizable Float where
    parameter = abs . ceiling

instance Parametrizable Double where
    parameter = abs . ceiling

instance Parametrizable [a] where
    parameter = length
