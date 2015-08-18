import "hint" HLint.Builtin.All
import "hint" HLint.Default
import "hint" HLint.Dollar

import Data.Map
import Control.Arrow
import Control.Monad
import Control.Monad.State

warn = (\ x -> f $ g x) ==> f . g

-- List
warn = cycle [c] ==> repeat c

warn = x == [] ==> null x where note = "generalize"
warn = [] == x ==> null x where note = "generalize"

warn = x /= [] ==> not (null x) where note = "generalize"
warn = [] /= x ==> not (null x) where note = "generalize"

warn = fst (unzip x) ==> map fst x
warn = snd (unzip x) ==> map snd x

warn "Use not . null" = length x > 0 ==> not (null x) where note = "increases laziness"
warn "Use not . null" = length x >= 1 ==> not (null x) where note = "increases laziness"

-- Map
warn = map fst (Data.Map.toList x) ==> Data.Map.keys x
warn = map snd (Data.Map.toList x) ==> Data.Map.elems x

warn = foldr f v (Data.Map.elems x) ==> Data.Map.foldr f v x
warn = Data.Maybe.fromMaybe d (Data.Map.lookup k m) ==> Data.Map.findWithDefault d k m

-- Arrows

warn = id *** id ==> id
warn = Control.Arrow.first id ==> id
warn = Control.Arrow.second id ==> id

warn = Control.Arrow.first f (Control.Arrow.second g x) ==> (f Control.Arrow.*** g) x
warn = Control.Arrow.second f (Control.Arrow.first g x) ==> (g Control.Arrow.*** f) x

warn = Control.Arrow.first f (Control.Arrow.first g x) ==> Control.Arrow.first (f . g) x
warn = Control.Arrow.second f (Control.Arrow.second g x) ==> Control.Arrow.second (f . g) x

warn = (a *** b) ((c &&& d) x) ==> ((a . c) Control.Arrow.&&& (b . d)) x
warn = (a *** b) ((c *** d) x) ==> ((a . c) Control.Arrow.*** (b . d)) x

warn = Control.Arrow.first f ((a *** b) x) ==> ((f . a) Control.Arrow.*** b) x
warn = Control.Arrow.second f ((a *** b) x) ==> (a Control.Arrow.*** (f . b)) x
warn = (a *** b) (Control.Arrow.first f x) ==> ((a . f) Control.Arrow.*** b) x
warn = (a *** b) (Control.Arrow.second f x) ==> (a Control.Arrow.*** (b . f)) x

warn = Control.Arrow.first f ((a &&& b) x) ==> ((f . a) Control.Arrow.&&& b) x
warn = Control.Arrow.second f ((a &&& b) x) ==> (a Control.Arrow.&&& (f . b)) x

-- State

warn = fmap f Control.Monad.State.get ==> Control.Monad.State.gets f
warn = Control.Monad.liftM f Control.Monad.State.get ==> Control.Monad.State.gets f
