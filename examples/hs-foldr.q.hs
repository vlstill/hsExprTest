import qualified Prelude
-- @ INJECT BEGIN
import Prelude hiding ( foldr, foldl, scanr, scanl, foldr1, foldl1, scanr1, scanl1 )
-- @ INJECT END

expr = "myfoldr"
timeout = 4

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr = Prelude.foldr
