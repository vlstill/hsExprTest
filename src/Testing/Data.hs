{-# LANGUAGE ExplicitForAll
           , ScopedTypeVariables
           , DeriveDataTypeable
           #-}
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

data DataDecl = DataDecl String [DataCon]
                deriving ( Eq, Ord, Show, Read, Data, Typeable )

data DataCon = DataCon String [String]
               deriving ( Eq, Ord, Show, Read, Data, Typeable )

-- | infer data declaration of given type
-- it works well for monomorphic ATDs, for polymorphic types
-- it will print data declaration as it would seem if all type
-- variables were instantiated.
-- For records it ommits record fields
-- For non-ADT types results will be wrong most likely
reflectData :: forall a. Data a => a -> DataDecl
reflectData witness = DataDecl name ctors
  where
    typ = dataTypeOf witness
    rep = dataTypeRep typ
    name = tyconUQname $ dataTypeName typ
    ctors = case rep of
        AlgRep ctors -> map (conVal >>> reflectCtor) ctors
        other        -> error "Not ADT"
    conVal :: Constr -> a
    conVal = fromConstr

showData :: DataDecl -> String
showData (DataDecl tyCon daCons) = unwords [ "data", tyCon, "= " ]
                                   ++ intercalate " | " (map showCon daCons)

showCon :: DataCon -> String
showCon (DataCon daCon args) = unwords (daCon : args)

conArgs :: Data a => a -> [TypeRep]
conArgs = gmapQ typeOf

conArgsNames :: Data a => a -> [String]
conArgsNames = conArgs >>> map show

constrArgs :: forall witness. Data witness => witness -> Constr -> [TypeRep]
constrArgs _ con = conArgs (fromConstr con :: witness)

constrArgsNames :: forall witness. Data witness => witness -> Constr -> [String]
constrArgsNames _ con = conArgsNames (fromConstr con :: witness)

reflectCtor :: Data a => a -> DataCon
reflectCtor witness = DataCon (showConstr (toConstr witness)) (conArgsNames witness)

showCtorDecl :: Data a => a -> String
showCtorDecl witness = case reflectCtor witness of
                        DataCon name args -> unwords $ name : args


canonizeData :: DataDecl -> DataDecl
canonizeData (DataDecl tyCon daCons) = DataDecl tyCon (sort daCons)
