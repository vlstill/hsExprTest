{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- (c) 2018 Vladimír Štill

module Data.Tuple.TH.Curry where

import Data.Tuple.TH.GenCurry

$(genCurries 16)
$(genUncurries 16)
