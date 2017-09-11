-- @ expr: myfoldr
-- @ limit: 2
-- @ inject

import qualified Prelude
-- @ INJECT BEGIN
import Prelude hiding ( foldr, foldl, scanr, scanl, foldr1, foldl1, scanr1, scanl1 )
-- @ INJECT END

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr = Prelude.foldr
