{-# LANGUAGE LambdaCase, TemplateHaskell, TupleSections #-}

-- Utilities for unit tests.
--
-- (c) 2018 Vladimír Štill

module Testlib ( getTests, wrapTest ) where

import Language.Haskell.TH ( Q, Exp (..), Lit (..), Name, reifyModule
                           , thisModule, lookupValueName )
import Language.Haskell.TH.Syntax ( Module (..), ModName (..), ModuleInfo (..) )
import Data.Maybe ( mapMaybe )
import Text.Printf.Mauke.TH ( sprintf )

-- | Find all 'test' functions exported by modules from this packages imported
--  into the module in which this splice is executed.
-- For found tests @t1, t2, ...@ from modules @M1, M2, ...@ it produces
-- an expression @[ wrapTest "M1" t1, wrapTest "M2" t2, ... ]@.
getTests :: Q Exp
getTests = do
    self@(Module selfpkg _) <- thisModule
    let getName (Module pkg (ModName name))
          | pkg == selfpkg        = Just name
          | otherwise             = Nothing

        testName imp = imp ++ ".test"

        testInfo :: String -> Q (Maybe (String, Name))
        testInfo imp = fmap (imp, ) <$> lookupValueName (testName imp)

        testCallE mod test = (VarE 'wrapTest `AppE` (LitE (StringL mod))) `AppE` VarE test

    imports <- mapMaybe getName . (\(ModuleInfo x) -> x) <$> reifyModule self
    tests <- mapMaybe id <$> mapM testInfo imports
    pure . ListE $ map (uncurry testCallE) tests

-- | Print test name before it is run and OK/FAILED after it ended.
wrapTest :: String -> IO Bool -> IO Bool
wrapTest name test = putStr (name ++ " ") >> test >>= \case
        True  -> putStrLn " OK" >> pure True
        False -> putStrLn ($(sprintf "\n%s FAILED") name) >> pure False
