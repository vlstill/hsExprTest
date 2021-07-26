{-# LANGUAGE Trustworthy #-}

import System.IO.Unsafe ( unsafePerformIO )
import System.Exit ( exitSuccess )

foo = unsafePerformIO $ exitSuccess

