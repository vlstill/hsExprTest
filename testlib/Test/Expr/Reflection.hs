{-# LANGUAGE TemplateHaskellQuotes, LambdaCase #-}

module Test.Expr.Reflection (
    module Language.Haskell.TH,
    -- * Assertion types
    ASTCheck,
    -- * Declaration checks
    isComprehentionD,
    callStartsWithOneOfD, callStartsWithOneOfD',
    callStartsWithOneOfE, callStartsWithOneOfE',
    onFunD,
    onFunBodyExp,
    singleClauseFunWithoutGuards,
    hasConstructorE
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.PprLib ( Doc )
import Data.Data ( gmapT, gmapQl, Data, Constr, toConstr )
import Data.Maybe ( fromJust )
import Data.Typeable ( cast )

-- | Any function with type 'ASTCheck' declared in the teacher's file will be
-- executed if reflection is enabled
type ASTCheck = [Dec] -> Q ()

-- | Fails if a declaration is not a list comprehention
isComprehentionD :: Dec -> Q ()
isComprehentionD (ValD (VarP _) body _) = matchCInBody body
isComprehentionD (ValD _ _ _)   = fail "Only plain variable declarations (without pattern matching) allowed"
isComprehentionD (FunD _ [Clause _ b _]) = matchCInBody b
isComprehentionD (FunD _ _)              = fail "Forbidden multiline declaration"
isComprehentionD _            = pure ()

matchCInBody :: Body -> Q ()
matchCInBody (NormalB ex) = case ex of
                                CompE _ -> pure ()
                                _       -> fail "Function must contain only a list comprehention"
matchCInBody _            = fail "Forbidden guards"

-- | Runs given check on function declarations of given name (on the actual
-- definition lines, i.e. 'FunD' or 'ValD', not on type signature). The
-- function must exist, otherwise 'fail' is raised.
onFunD :: [Dec] -> String -> (Dec -> Q ()) -> Q ()
onFunD decs name fn = mapM_ fn . assertExists $ filter select decs
  where
    select :: Dec -> Bool
    select (FunD (Name (OccName n) _) _)          = n == name
    select (ValD (VarP (Name (OccName n) _)) _ _) = n == name
    select _                                      = False

    assertExists [] = fail $ "Missing mandatory function `" ++ name ++ "'"
    assertExists x  = x

disallowdDec :: Dec -> Q ()
disallowdDec d = fail $ "Disallowed declaration " ++ compactPpr d

onFunBodyExp :: Dec -> (Exp -> Q ()) -> Q ()
onFunBodyExp dec check = case dec of
    FunD _ cls    -> mapM_ (\(Clause _ body _) -> onBody body) cls
    ValD _ body _ -> onBody body
    d             -> disallowdDec d
  where
    onBody (GuardedB grds) = mapM_ (check . snd) grds
    onBody (NormalB ex)   = check ex


-- | Accepts a function which is defined in term of given names. The function
-- must not have multiline declarations. The first argument specifies if
-- functional composition is allowed.
callStartsWithOneOfD' :: Bool -> [Name] -> Dec -> Q ()
callStartsWithOneOfD' allowComposition names dec = do
    singleClauseFunWithoutGuards dec
    case dec of
        ValD _ (NormalB expr) _            -> check expr
        FunD _ [Clause _ (NormalB expr) _] -> check expr
        d                                  -> disallowdDec d
  where
    check expr = callStartsWithOneOfE' allowComposition names expr

singleClauseFunWithoutGuards :: Dec -> Q ()
singleClauseFunWithoutGuards dec = case dec of
    ValD (VarP _) body _     -> check body
    ValD _ _ _               -> fail "Only plain variable declarations (without pattern matching) allowed"
    FunD _ [Clause _ body _] -> check body
    FunD _ _                 -> fail "Forbidden multiline declaration"
    d                        -> disallowdDec d
  where
    check (NormalB _) = pure ()
    check _              = fail "Forbidden guards"

-- | Version of 'callStartsWithOneOfD'' which does not accept function composition
callStartsWithOneOfD :: [Name] -> Dec -> Q ()
callStartsWithOneOfD = callStartsWithOneOfD' False

-- Accepts an expression which is defined in term of given names. The first
-- argument specifies if functional composition is allowed.
callStartsWithOneOfE' :: Bool -> [Name] -> Exp -> Q ()
callStartsWithOneOfE' allowComposition names expr = go expr
  where
    go (AppE fn arg)
      | VarE dot <- fn, dot == '(.)     = if allowComposition then go arg
                                             else err dot
      | otherwise                       = go fn
    go e@(InfixE (Just l) (VarE dot) _)
      | dot == '(.)                     = if allowComposition then go l
                                            else err dot
      | otherwise                       = err e
    go (VarE n)
      | n `elem` names                  = pure ()
      | otherwise                       = err n
    go e                                = reportError "go e" >> err e

    err n = fail $ "Found `" ++ compactPpr n ++ "', expected expression starting with one of "
                   ++ show (map compactPpr' names)

-- | Check if the given expression contains a subexpression created by given constructor
hasConstructorE :: Constr -> String -> Exp -> Q ()
hasConstructorE constr msg expr0 = if go expr0 then pure () else fail msg
  where
    go :: Exp -> Bool
    go expr
      | toConstr expr == constr = True
      | InfixE a b c <- expr    = mGo a || go b || mGo c
      | otherwise               = gmapQl (||) False
                                         (\x -> case cast x of
                                                Nothing -> False
                                                Just ex -> go (ex :: Exp))
                                         expr

    mGo Nothing = False
    mGo (Just ex) = go ex


compactPpr' :: (Data a, Ppr a) => a -> Doc
compactPpr' = ppr . stripNames
  where
    stripNames :: Data a => a -> a
    stripNames d = case cast d of
                      Nothing -> gmapT stripNames d
                      Just (Name name _) -> fromJust . cast $ Name name NameS

compactPpr :: (Data a, Ppr a) => a -> String
compactPpr = show . compactPpr'

-- | Version of 'callStartsWithOneOfE'' which does not accept function composition
callStartsWithOneOfE :: [Name] -> Exp -> Q ()
callStartsWithOneOfE = callStartsWithOneOfE' False
