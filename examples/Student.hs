{-# LANGUAGE Safe, NoTemplateHaskell #-}
module Student where

{-# LINE 1 "Student.hs" #-}
{-# LINE 1 "Inject.hs" #-}

{-# LINE 1 "Student.hs" #-}
foo :: a -> a
foo x = foo x
