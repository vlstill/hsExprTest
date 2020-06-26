{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- (c) 2018 Vladimír Štill

module Data.Tuple.TH.Curry where

import Data.Tuple.TH.GenCurry

-- Since GHC 8.10 the Tup* TH representations with 1 argument will generate a
-- signleton tuple (Unit) instead of being an identity. Since we need
-- (un)curry1 for consystency reasons in the expression, we will write them
-- manually to ensure compatibility with new and old GHCs alike.

curry1 :: (t1 -> t2) -> t1 -> t2
curry1 f = f

uncurry1 :: (t1 -> t2) -> t1 -> t2
uncurry1 f = f

$(genCurries 16)
$(genUncurries 16)
