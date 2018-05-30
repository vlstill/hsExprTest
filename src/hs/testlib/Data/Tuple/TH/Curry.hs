{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Data.Tuple.TH.Curry where

import Data.Tuple.TH.GenCurry

$(genCurries 62)
$(genUncurries 62)
