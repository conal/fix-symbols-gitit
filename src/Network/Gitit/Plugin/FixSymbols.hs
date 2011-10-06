{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Network.Gitit.Plugin.FixSymbols
-- Copyright   :  (c) Conal Elliott 2010
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Turn some Haskell symbols into pretty math symbols
-- Many are selected from <http://xahlee.org/Periodic_dosage_dir/unicode.html>
-- 
-- TODO: move specialized rewrites out of substMap. Instead, allow a
-- metadata field, such as:
-- 
--   substMap: [("abutWE","⇔"), ("abutSN","⇕")]
-- 
----------------------------------------------------------------------

module Network.Gitit.Plugin.FixSymbols
  ( plugin, rewriter, fixInline, fixBlock
  ) where

import Network.Gitit.Interface

import Data.Char (isUpper)
import Data.Maybe (fromMaybe)
import Data.List (isSuffixOf)

import qualified Data.Map as Map
import Data.Map (Map)

type Unop a = a -> a

rewriter :: Unop Pandoc
rewriter = bottomUp fixInline . bottomUp fixBlock

plugin :: Plugin
plugin = PageTransform $ return . rewriter

-- mkPageTransform :: Data a => (a -> a) -> Plugin
-- mkPageTransform fn = PageTransform $ return . bottomUp fn

fixInline :: Unop Inline
fixInline (Code attr@(_,classes,_) s)
  | "url" `notElem` classes = Code attr (translate s)
fixInline x             = x

-- The url exception is thanks to Travis Cardwell.

fixBlock :: Unop Block
fixBlock (CodeBlock attr@(_,classes,_) s)
  | "haskell" `elem` classes = CodeBlock attr (translate (dropBogus s))
fixBlock x = x

-- Drop lines that end with: error "bogus case pending compiler fix"
-- To do: maybe move to another plugin
dropBogus :: Unop String
dropBogus = unlines . filter (not . bogus) . lines

bogus :: String -> Bool
bogus = isSuffixOf "error \"bogus case pending compiler fix\""

translate :: Unop String
translate = concat . fixInfix . map translateLex . fixLex . lexString

-- Turn "`(+)`" into "+", but before concat'ing
fixInfix :: Unop [String]
fixInfix [] = []
fixInfix ("`":s:"`":ss) | Just op <- stripParens s = op : fixInfix ss
fixInfix (s : ss) = s : fixInfix ss

isQuantifier :: String -> Bool
isQuantifier = (`elem` ["forall","exists"])

-- Misc tweaks on lexeme streams, including determining whether a "." is a
-- function composition, part of forall, a qualified name, or the end of a
-- sentence in a comment.
fixLex :: Unop [String]
fixLex [] = []
fixLex ("[":"|":ss) = "[|" : fixLex ss   -- semantic bracket
fixLex ("|":"]":ss) = "|]" : fixLex ss
fixLex (ss@(q:_)) | isQuantifier q = before ++ (dotLex : fixLex after) -- forall a b c. ...
 where
   (before,(".":after)) = break (== ".") ss
fixLex (s@(c:_):".":ss) | isUpper c = fixLex ((s++"."):ss) -- qualified name
-- fixLex (s@(c:_):".":ss)
--   | not (isSpace c) && (null ss || isSpace (head (head ss))) = fixLex ((s++"."):ss) -- end of sentence
fixLex (s:".":ss@(" ":_)) | s /= " " = s : dotLex : fixLex ss -- end of sentence
fixLex (s : ss) = s : fixLex ss

dotLex :: String
dotLex = "#dot#"

stripParens :: String -> Maybe String
stripParens ('(':s) | not (null s) && last s == ')' = Just (init s)
stripParens _ = Nothing

translateLex :: Unop String
translateLex s = fromMaybe s $ Map.lookup s substMap 

{- 

I tried thin spaces (http://www.cs.tut.fi/~jkorpela/chars/spaces.html)
to make up for substituted symbols often being shorter.

three-per-em space 	foo bar
four-per-em space 	foo bar
six-per-em space 	foo bar
hair space	 	foo bar

However, standard fat spaces get substituted for these thin ones.

-}

substMap :: Map String String
substMap = Map.fromList $
  [ ("<=","≤"), (">=", "≥")
  , ("forall","∀"),("exists","∃"),(dotLex,".")
  , ("undecided", "…")
  , ("->","→"),(".","∘"),(":*","×"),("=>","⇒"), ("<==>","⟺") -- or "⇔"
  , (":*:","×"), (":+:","+"), (":.","∘")
  , ("\\","λ")
  , ("lub","(⊔)"),("glb","(⊓)")
  , ("mempty","∅"), ("mappend","(⊕)"), ("op","(⊙)")
  -- , ("<*>","⊛")
  , ("undefined","⊥"), ("bottom","⊥")
  , ("<-","←"), ("::","∷"), ("..","‥"), ("...","⋯")
  , ("==","≡"), ("/=","≠")
  , ("=~","≅")
  , (":->", "↣"), (":->:","↛") -- or: ⇉, ⇥
  , (":-+>", "☞"), ("-->", "⇢"), ("~>", "↝"),("~>*", "↝*") -- or ⇨
  , (":^+", "➴"), (":+^", "➶") -- top-down vs bottom-up comp -- ↥ ↧ ↱↰ ↥ ↧ ⇤ ⇥ ⤒ ↱ ↲ ↳ ↰ ➷ ➸ ➹
  , ("[|","⟦"), ("|]","⟧")  -- semantic brackets
  , ("||","∨"), ("&&","∧") -- maybe
  , ("abutWE","⇔"), ("abutSN","⇕")

  , ("alpha","α") , ("beta","β") , ("gamma","γ") , ("delta","δ")
  , ("epsilon","ε") , ("zeta","ζ") , ("eta","η") , ("theta","θ")
  , ("iota","ι") , ("kappa","κ") , ("lambda","λ") , ("mu","μ") , ("nu","ν")
  , ("xi","ξ") , ("omicron","ο") , ("pi","π") , ("rho","ρ") , ("sigma","σ")
  , ("tau","τ") , ("upsilon","υ") , ("phi","φ") , ("chi","χ") , ("psi","ψ")
  , ("omega","ω")

  , ("Alpha","Α") , ("Beta","Β") , ("Gamma","Γ") , ("Delta","Δ")
  , ("Epsilon","Ε") , ("Zeta","Ζ") , ("Eta","Η") , ("Theta","Θ")
  , ("Iota","Ι") , ("Kappa","Κ") , ("Lambda","Λ") , ("Mu","Μ") , ("Nu","Ν")
  , ("Xi","Ξ") , ("Omicron","Ο") , ("Pi","Π") , ("Rho","Ρ") , ("Sigma","Σ")
  , ("Tau","Τ") , ("Upsilon","Υ") , ("Phi","Φ") , ("Chi","Χ") , ("Psi","Ψ")
  , ("Omega","Ω")

  ]

-- The 'reverse' is to apply earlier rewrites first.  Or flip (.)

-- Experiments in string dissection. Simplified lexing.

-- | Dissect a string of haskell code into lexemes.
-- Mainly uses Prelude's 'lex', but preserves spaces.
lexString :: String -> [String]
lexString "" = []
lexString (c:s') | c `elem` " \n\t" = [c] : lexString s'
lexString s | [(h,t)] <- lex s = h : lexString t
lexString (c:s') = [c] : lexString s'
