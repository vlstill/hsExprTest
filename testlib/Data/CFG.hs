{-# LANGUAGE UnicodeSyntax, ViewPatterns, NamedFieldPuns, TupleSections,
             MultiWayIf, BangPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Data.CFG
    -- * Terminals and Nonterminals
    ( TermOrNonterm (Term, Nonterm)
    , isTerminal, isNonterminal, getTerminal, getNonterminal
    -- * Context-Free Grammars (CFGs)
    , CFG ( CFG ), rules
    , initial, terminals, nonterminals, symbols
    -- * Couting words
    , wordCount, wordCount'
    -- * Generating words
    , randomWord, randomWord'
    -- * Examples
    , example_anyAB
    , example_wwR
    ) where

import Control.Arrow
import Data.Function.Memoize
import Data.List ( find )
import Data.List.NonEmpty ( NonEmpty ( (:|) ), toList )
import Data.Maybe ( mapMaybe, maybeToList, isJust, fromMaybe )
import Data.Set ( Set, fromList )
import Prelude.Unicode
import System.Random

data TermOrNonterm t n = Term t
                       | Nonterm n
                       deriving (Eq, Ord, Show, Read)

isTerminal ∷ TermOrNonterm t n → Bool
isTerminal Term{} = True
isTerminal _      = False

isNonterminal ∷ TermOrNonterm t n → Bool
isNonterminal Nonterm{} = True
isNonterminal _         = False

getTerminal ∷ TermOrNonterm t n → Maybe t
getTerminal (Term t) = Just t
getTerminal _        = Nothing

getNonterminal ∷ TermOrNonterm t n → Maybe n
getNonterminal (Nonterm n) = Just n
getNonterminal _           = Nothing

-- | A representation of Context-Free Grammar with terminals chosen from type
-- @term@ and nonterminals from @nonterm@. Of course, for the grammar to be
-- context-free, the definition has to be finite.
-- Each rule is given by the source nonterminal and a list of rewrites which are
-- sequences of terminals and nonterminals.
-- The first rule corresponds to the starting nonterminal.
newtype CFG term nonterm =
    CFG { rules ∷ NonEmpty (nonterm, [[TermOrNonterm term nonterm]]) }
    deriving (Eq, Show, Read)

deriveMemoizable ''TermOrNonterm
deriveMemoizable ''NonEmpty
deriveMemoizable ''CFG


initial ∷ CFG term nonterm → nonterm
initial (rules → (x, _) :| _) = x

terminals ∷ Ord term ⇒ CFG term n → Set term
terminals = symbols >>> mapMaybe getTerminal >>> fromList

nonterminals ∷ Ord nonterm ⇒ CFG t nonterm → Set nonterm
nonterminals = symbols >>> mapMaybe getNonterminal >>> fromList

symbols ∷ CFG t n → [TermOrNonterm t n]
symbols = rules >>> toList >>> map snd >>> concat >>> concat

-- | The grammar needs to be proper – without simple rules, without unusable
-- symbols (unreachable & non-normalized) and without epsilon rules. Otherwise,
-- the results will not make sense. Furthemore, the sentence of terminal and
-- nonterminal characters given in the second argument must be derivable from
-- the grammar.
--
-- Note: If you are going to call this function repeatedly for the same grammar,
-- you should bind @wordCount grammar@ to a local variable to use it
-- efficiently (this way, the function memoizes all calls over the same
-- grammar, speeding the calculation substantially).
wordCount ∷ (Eq n, Memoizable n, Memoizable t)
          ⇒ CFG t n → [TermOrNonterm t n] → Int → Integer
wordCount cfg = memoFix2 $ \wc sentence len →
    let slen = length sentence
    in case sentence of
      -- base: ε sentence
      []                            → b2i $ len ≡ 0
      -- base: special case for generating ε from initial nonterminal – the only
      -- shortening rule
      [Nonterm i]
        | i ≡ initial cfg ∧ len ≡ 0 → b2i . isJust $ null `find` initTargets
      _
        -- base: all other shortening is invalid
        | slen > len                → 0
        -- base:
        | all isTerminal sentence   → b2i $ len ≡ slen
      -- for a single nonterminal, explore all the ways it can be rewritten
      [Nonterm n]                   → sum
          [ wc target len | ts ← maybeToList (lookup n ruleList), target ← ts ]
      -- for a general sentence, split the first character and try all possible
      -- lenght combinations for the first character + the rest
      _ → sum
          [ cntAlpha * cntBeta
          | let (alpha, beta) = splitAt 1 sentence,
            a ← [1..len - 1], let b = len - a,
            let cntAlpha = wc alpha a,
            cntAlpha > 0,
            let cntBeta = wc beta b ]
  where
    ruleList = toList $ rules cfg

    b2i ∷ Bool → Integer
    b2i = fromIntegral . fromEnum

    (_, initTargets) :| _ = rules cfg

-- | A simplified version of 'wordCount' that calculculates number of words of
-- given length derivable from the initial nonterminal. The same prerequisites
-- must be met as for 'wordCount'.
--
-- Note: For efficient use, see the note in 'wordCount'.
wordCount' ∷ (Eq n, Memoizable n, Memoizable t)
          ⇒ CFG t n → Int → Integer
wordCount' cfg = wordCount cfg [Nonterm (initial cfg)]

-- | Retuns a random word of given lengths generated from the sentence given as
-- the second argument. For requirements on the CFG, see 'wordCount'.
--
-- Note: If you are going to call this function repeatedly for the same grammar,
-- you should bind @randomWord grammar@ to a local variable to use it
-- efficiently.
randomWord ∷ (RandomGen g, Eq n, Memoizable n, Memoizable t)
            ⇒ CFG t n → g → [TermOrNonterm t n] → Int → Maybe [t]
-- TODO: depsite all the lets and lambdas, all calculations over the same CFG
-- DO NOT appear to share the same memoizing 'wordCount'.
randomWord cfg = let wc = wordCount cfg in \gen0 sentence0 len →
  let
    go gen sentence =
        if | len ≡ 0 ∧ wc sentence 0 ≡ 1 → Just []
           | wc sentence len ≡ 0         → Nothing
           | all isTerminal sentence     → sequence $ getTerminal <$> sentence
           | otherwise →
              let (firstNontermIdx, firstNonterm) = head [ (i, n) | (i, Nonterm n) ← zip [0..] sentence ]
                  (prefix, _:suffix) = splitAt firstNontermIdx sentence
                  rlist = toList $ rules cfg
                  targets = fromMaybe [] $ lookup firstNonterm rlist
                  candidates = [ (weight, candidate)
                               | tgt ← targets,
                                 let ts = tgt ++ suffix,
                                 let weight = wc ts (len - firstNontermIdx),
                                 weight > 0,
                                 let candidate = prefix ++ ts
                               ]
                  (g1, g2) = split gen
              in if null candidates then Nothing else go g2 $ chooseWeighted g1 candidates

    chooseWeighted gen vals = select 0 cumulative
      where
        select lstW ((w, x) : wxs)
          | lstW < optionRaw ∧ optionRaw ≤ w = x
          | otherwise                            = select w wxs
        select _ _ = error "chooseWeighted: empty list"

        total = sum $ map fst vals
        cumulative = cum 0 vals
          where
            cum _   [] = []
            cum acc ((w0, x) : wxs) = let w = w0 + acc in (w, x) : cum w wxs
        optionRaw = fst $ uniformR (1, total) gen
  in go gen0 sentence0

-- | A version of 'randomWord' that generates from the initial nonterminal. See
-- 'randomWord' for more details.
randomWord' ∷ (RandomGen g, Eq n, Memoizable n, Memoizable t)
            ⇒ CFG t n → g → Int → Maybe [t]
randomWord' cfg = \gen → randomWord cfg gen [Nonterm (initial cfg)]
    

-- | An example grammar of ${ ww^R | w ∈ {a, b}* }$.
--
-- @
-- S → aSa | bSb | ε
-- @
example_wwR ∷ CFG Char Char
example_wwR = CFG $ ('S', [[Term 'a', Nonterm 'X', Term 'a'],
                           [Term 'b', Nonterm 'X', Term 'b'],
                           [Term 'a', Term 'a'],
                           [Term 'b', Term 'b'],
                           []
                          ])
                    :|
                    [('X', [[Term 'a', Nonterm 'X', Term 'a'],
                            [Term 'b', Nonterm 'X', Term 'b'],
                            [Term 'a', Term 'a'],
                            [Term 'b', Term 'b']
                          ])]

example_anyAB ∷ CFG Char Char
example_anyAB = CFG $ ('S', [[Term 'a', Nonterm 'S'],
                             [Term 'b', Nonterm 'S'],
                             [Term 'a'],
                             [Term 'b']
                            ]) :| []
