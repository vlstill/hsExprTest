import Control.Monad.IO.Class

empty :: [a]
empty = [ undefined | _ <- [] ]

liftIO (putStrLn "W: booo") >> pure []
