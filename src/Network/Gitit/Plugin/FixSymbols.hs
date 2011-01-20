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

import qualified Data.Map as Map
import Data.Map (Map)

plugin :: Plugin
plugin = PageTransform $ return . processWith fixInline . processWith fixBlock

-- mkPageTransform :: Data a => (a -> a) -> Plugin
-- mkPageTransform fn = PageTransform $ return . processWith fn


fixInline :: Inline -> Inline
fixInline (Code s) = Code (translate s)
fixInline x        = x

fixBlock :: Block -> Block
fixBlock (CodeBlock attr@(_,classes,_) s)
  | "haskell" `elem` classes = CodeBlock attr (translate s)
fixBlock x = x

translate :: String -> String
translate = concat . fixInfix . map translateLex . fixLex . lexString

-- Turn "`(+)`" into "+", but before concat'ing
fixInfix :: [String] -> [String]
fixInfix [] = []
fixInfix ("`":s:"`":ss) | Just op <- stripParens s = op : fixInfix ss
fixInfix (s : ss) = s : fixInfix ss

-- Misc tweaks on lexeme streams
fixLex :: [String] -> [String]
fixLex [] = []
fixLex ("[":"|":ss) = "[|" : fixLex ss   -- semantic bracket
fixLex ("|":"]":ss) = "|]" : fixLex ss
fixLex (s@(c:_):".":ss) | isUpper c = fixLex ((s++"."):ss) -- qualified name
fixLex (s : ss) = s : fixLex ss

stripParens :: String -> Maybe String
stripParens ('(':s) | not (null s) && last s == ')' = Just (init s)
stripParens _ = Nothing

translateLex :: String -> String
translateLex s = fromMaybe s $ Map.lookup s substMap 

substMap :: Map String String
substMap = Map.fromList $
  [ ("forall","∀"),("->","→"),(".","∘"),(":*","×")
  , (":*:","×"), (":+:","+"), (":.","∘")
  , ("\\","λ")
  , ("lub","(⊔)"),("glb","(⊓)")
  , ("mempty","∅"), ("mappend","(⊕)")
  , ("undefined","⊥"), ("bottom","⊥")
  , ("<-","←"), ("::","∷"), ("..","‥"), ("...","⋯")
  , ("==","≡"), ("/=","≠")
  , ("≅","=~")
  , (":->", "↣"), (":->:","↠")
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
