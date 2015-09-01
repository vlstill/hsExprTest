{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, DeriveDataTypeable #-}

-- | Support for testing data type declarations.
--
-- (c) 2014 Vladimír Štill

module Testing.Data
    ( showData
    , reflectData
    , conArgs
    , conArgsNames
    , constrArgs
    , constrArgsNames
    , DataDecl (DataDecl)
    , DataCon (DataCon)
    , canonizeData
    , deepCanonizeData
    ) where

import Data.Data
import Control.Arrow
import Data.List

-- | Representation of (monomorphic) data type.
data DataDecl = DataDecl { dataDeclName :: String
                         , dataDeclCons :: [DataCon]
                         }
                deriving ( Eq, Ord, Show, Read, Data, Typeable )

-- | Representatio of data constructor.
data DataCon = DataCon { dataConName   :: String
                       , dataConFields :: [String]
                       }
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
        AlgRep c -> map (conVal >>> reflectCtor) c
        _        -> error "Not ADT"
    conVal :: Constr -> a
    conVal = fromConstr

-- | Get string representation of 'DataDecl'.
showData :: DataDecl -> String
showData (DataDecl tyCon daCons) = unwords [ "data", tyCon, "= " ]
                                   ++ intercalate " | " (map showCon daCons)

showCon :: DataCon -> String
showCon (DataCon daCon args) = unwords (daCon : map paren args)
  where paren x = if ' ' `elem` x then '(' : x ++ ")" else x

-- | Get types of constructor arguments of given value.
conArgs :: Data a => a -> [TypeRep]
conArgs = gmapQ typeOf

-- | Get string types of constructor arguments.
conArgsNames :: Data a => a -> [String]
conArgsNames = conArgs >>> map show

-- | As 'conArgs' but value is build from given type witness and constuctor
-- representation.
constrArgs :: forall witness. Data witness => witness -> Constr -> [TypeRep]
constrArgs _ con = conArgs (fromConstr con :: witness)

-- | As 'conArgsNames' but value is build from given type witness and constuctor
-- representation.
constrArgsNames :: forall witness. Data witness => witness -> Constr -> [String]
constrArgsNames _ con = conArgsNames (fromConstr con :: witness)

reflectCtor :: Data a => a -> DataCon
reflectCtor witness = DataCon (showConstr (toConstr witness)) (conArgsNames witness)

-- | Get canonical representation of data type, that is such that data 
-- constructor order is sorted.
canonizeData :: DataDecl -> DataDecl
canonizeData (DataDecl tyCon daCons) = DataDecl tyCon (sort daCons)

-- | Canonize also data field order, in addition to data constructor
-- order (as done by 'canonizeData').
deepCanonizeData :: DataDecl -> DataDecl
deepCanonizeData (DataDecl tyCon daCons) = canonizeData $ DataDecl tyCon (map sortCtor daCons)
  where
    sortCtor (DataCon name types) = DataCon name (sort types)
