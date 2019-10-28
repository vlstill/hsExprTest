module Test.Expr.Reflection (
    module Language.Haskell.TH,
    -- * Assertion types
    ASTCheck,
    -- * Declaration checks
    isComprehentionD
    ) where

import Language.Haskell.TH

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
