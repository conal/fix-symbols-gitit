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
----------------------------------------------------------------------

module Network.Gitit.Plugin.FixSymbols
  ( plugin, fixInline, fixBlock
  ) where

import Network.Gitit.Interface

import Data.Char (isUpper)
import Data.Maybe (fromMaybe)
import Data.List (isSuffixOf)

import qualified Data.Map as Map
import Data.Map (Map)

type Unop a = a -> a

plugin :: Plugin
plugin = PageTransform $ return . processWith fixInline . processWith fixBlock

-- mkPageTransform :: Data a => (a -> a) -> Plugin
-- mkPageTransform fn = PageTransform $ return . processWith fn

fixInline :: Unop Inline
fixInline (Code s) = Code (translate s)
fixInline x        = x

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

-- Misc tweaks on lexeme streams, including determining whether a "." is a
-- function composition or part of forall or a qualified name.
fixLex :: Unop [String]
fixLex [] = []
fixLex ("[":"|":ss) = "[|" : fixLex ss   -- semantic bracket
fixLex ("|":"]":ss) = "|]" : fixLex ss
fixLex (ss@("forall":_)) = before ++ (dotLex: fixLex after) -- forall a b c. ...
 where
   (before,(".":after)) = break (== ".") ss
fixLex (s@(c:_):".":ss) | isUpper c = fixLex ((s++"."):ss) -- qualified name
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
  [ ("forall","∀"),(dotLex,"."),("->","→"),(".","∘"),(":*","×"),("=>","⇒")
  , (":*:","×"), (":+:","+"), (":.","∘")
  , ("\\","λ")
  , ("lub","(⊔)"),("glb","(⊓)")
  , ("mempty","∅"), ("mappend","(⊕)")
  , ("<*>","⊛")
  , ("undefined","⊥"), ("bottom","⊥")
  , ("<-","←"), ("::","∷"), ("..","‥"), ("...","⋯")
  , ("==","≡"), ("/=","≠")
  , ("=~","≅")
  , (":->", "↣"), (":->:","↛") -- or: ⇉, ⇥
  , (":-+>", "☞"), ("-->", "⇢"), ("~>", "↝") -- or ⇨
  , ("[|","⟦"), ("|]","⟧")  -- semantic brackets

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
