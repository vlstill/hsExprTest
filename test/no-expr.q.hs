-- @ compiled: True

import qualified Student
import System.Exit ( exitSuccess, exitFailure )

evaluator :: () -> IO ()
evaluator _ = if Student.b then exitSuccess else exitFailure
