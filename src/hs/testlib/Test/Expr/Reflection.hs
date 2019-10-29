{-# LANGUAGE TemplateHaskellQuotes #-}

module Test.Expr.Reflection (
    module Language.Haskell.TH,
    -- * Assertion types
    ASTCheck,
    -- * Declaration checks
    isComprehentionD,
    callStartsWithOneOfD, callStartsWithOneOfD',
    callStartsWithOneOfE, callStartsWithOneOfE',
    onFunD
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax


-- | Any function with type 'ASTCheck' declared in the teacher's file will be
-- executed if reflection is enabled
type ASTCheck = [Dec] -> Q ()

-- | Fails if a declaration is not a list comprehention
isComprehentionD :: Dec -> Q ()
isComprehentionD (ValD (VarP _) body _) = matchCInBody body
isComprehentionD (ValD _ _ _)   = fail "Only plain variable declarations (without pattern matching) allowed"
isComprehentionD (FunD _ cls)
              | length cls == 1 = let [Clause _ b _] = cls in matchCInBody b
              | otherwise       = fail "Forbidden multiline declaration"
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


-- | Accepts a function which is defined in term of given names. The function
-- must not have multiline declarations. The first argument specifies if
-- functional composition is allowed.
callStartsWithOneOfD' :: Bool -> [Name] -> Dec -> Q ()
callStartsWithOneOfD' allowComposition names dec = case dec of
    ValD (VarP _) body _     -> csB body
    ValD _ _ _               ->  fail "Only plain variable declarations (without pattern matching) allowed"
    FunD _ [Clause _ body _] -> csB body
    FunD _ _                 -> fail "Forbidden multiline declaration"
    d                        -> fail $ "Disallowed declaration " ++ show (ppr d)
  where
    csB (NormalB expr) = callStartsWithOneOfE' allowComposition names expr
    csB _              = fail "Forbidden guards"

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

    err n = fail $ "Found `" ++ show (ppr n) ++ "', expected expression starting with one of "
                   ++ show (map (\(Name (OccName x) _) -> x) names)

-- | Version of 'callStartsWithOneOfE'' which does not accept function composition
callStartsWithOneOfE :: [Name] -> Exp -> Q ()
callStartsWithOneOfE = callStartsWithOneOfE' False
