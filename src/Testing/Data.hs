{-# LANGUAGE ExplicitForAll
           , ScopedTypeVariables #-}
module Testing.Data
    ( showData
    , conArgs
    , conArgsNames
    , constrArgs
    , constrArgsNames
    ) where

import Data.Data
import Control.Arrow
import Data.List

-- | infer data declaration of given type
-- it works well for monomorphic ATDs, for polymorphic types
-- it will print data declaration as it would seem if all type
-- variables were instantiated.
-- For records it ommits record fields
-- For non-ADT types results will be wrong most likely
showData :: forall a. Data a => a -> String
showData witness = unwords [ "data", name ] ++ constring
  where
    typ = dataTypeOf witness
    rep = dataTypeRep typ
    name = tyconUQname $ dataTypeName typ
    constring = case rep of
        AlgRep ctors -> ($ ctors) $ map (fromConstr :: Constr -> a) >>>
                        map showCtorDecl >>>
                        intercalate "\n    | " >>>
                        ("\n    = " ++)
        other        -> " = " ++ show other
    indent = ("    " :)

    conVal :: Constr -> a
    conVal = fromConstr

conArgs :: Data a => a -> [TypeRep]
conArgs = gmapQ typeOf

conArgsNames :: Data a => a -> [String]
conArgsNames = conArgs >>> map show

constrArgs :: forall witness. Data witness => witness -> Constr -> [TypeRep]
constrArgs _ con = conArgs (fromConstr con :: witness)

constrArgsNames :: forall witness. Data witness => witness -> Constr -> [String]
constrArgsNames _ con = conArgsNames (fromConstr con :: witness)

showCtorDecl :: Data a => a -> String
showCtorDecl witness = unwords $ showConstr (toConstr witness)
                                 : conArgsNames witness
