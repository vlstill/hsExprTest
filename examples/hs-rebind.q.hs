-- @ expr: myif
-- @ opts: RebindableSyntax
-- @ timeout: 4
--
-- this example demonstrates how rebindable syntax can be used to disable
-- syntax constructs such as if-then-else

-- @ INJECT BEGIN
import Prelude ( Bool ( True, False ) )
-- @ INJECT END

myif :: Bool -> a -> a -> a
myif True  x _ = x
myif False _ y = y
