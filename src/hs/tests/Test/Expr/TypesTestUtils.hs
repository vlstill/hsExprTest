{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

-- (c) 2018 Vladimír Štill

module Test.Expr.TypesTestUtils where

import Test.Expr.Types
import Test.Expr.Utils ( spliceFileLoc )
import Language.Haskell.TH ( Q, Type, Exp, Pat, pprint )
import Control.Applicative ( liftA2 )
import Text.Printf.Mauke.TH ( sprintf )

data TestSuccess = TS
data TestFailure = TF

simplifyRet :: Either (UniTypeId, String) (TypeOrder, Type) -> Either UniTypeId TypeOrder
simplifyRet (Left (a, _))  = Left a
simplifyRet (Right (a, _)) = Right a

testUnify :: Q Pat -> Q Type -> Q Type -> Q Exp
testUnify pat t1 t2 = do
    (ppat :: String) <- pprint <$> pat
    (pt1 :: String) <- pprint <$> t1
    (pt2 :: String) <- pprint <$> t2
    uni <- liftA2 unify t1 t2
    loc <- spliceFileLoc
    let res = simplifyRet uni
        sres = show res
        pres = (either (const "") (pprint . snd) uni)
    [| case res of
            $(pat) -> putStr "." >> pure True
            _      -> putStrLn ($(sprintf "\nunification test failed:\n    %s\n    `unify`\n    %s\n    =\n    %s\n\n        expected %s\n        got      %s\n        at       %s")
                                  pt1 pt2 (pres :: String) ppat sres loc) >> pure False
     |]
