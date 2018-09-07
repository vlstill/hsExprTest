-- @ hsstring

import Student ( input )
import System.Exit ( exitFailure )
import Control.Monad ( when )

main = when (i /= "foo") exitFailure
  where
    i = reverse . dropWhile (== '\n') $ reverse input
