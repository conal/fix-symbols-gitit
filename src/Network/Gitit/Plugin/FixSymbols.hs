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
translate = concat . fixInfix . map translateLex . lexString

-- Turn "`(+)`" into "+", but before concat'ing
fixInfix :: [String] -> [String]
fixInfix [] = []
fixInfix ("`":s:"`":ss) | Just op <- stripParens s = op : fixInfix ss
fixInfix (s : ss) = s : fixInfix ss

stripParens :: String -> Maybe String
stripParens ('(':s) | not (null s) && last s == ')' = Just (init s)
stripParens _ = Nothing

translateLex :: String -> String
translateLex s = fromMaybe s $ Map.lookup s substMap 

substMap :: Map String String
substMap = Map.fromList $
  [ ("forall","∀"),("->","→"),(":*","×")
  , ("\\","λ")
  , ("lub","(⊔)"),("glb","(⊓)")
  , ("undefined","⊥"), ("bottom","⊥")
  , ("<-","←"), ("::","∷"), ("..","‥"), ("...","⋯")
  , ("==","≡"), ("/=","≠")
  
  , ("alpha", "α"), ("iota", "ι"), ("varrho", "ϱ"), ("beta", "β")
  , ("kappa", "κ"), ("sigma", "σ"), ("gamma", "γ"), ("lambda", "λ")
  , ("varsigma", "ς"), ("delta", "δ"), ("mu", "μ"), ("tau", "τ")
  , ("epsilon", "ϵ"), ("nu", "ν"), ("upsilon", "υ"), ("varepsilon", "ε")
  , ("xi", "ξ"), ("phi", "ϕ"), ("zeta", "ζ"), ("o", "ο"), ("varphi", "φ")
  , ("eta", "η"), ("pi ", "π"), ("chi", "χ"), ("theta", "θ"), ("varpi", "ϖ")
  , ("psi", "ψ"), ("vartheta", "ϑ"), ("rho", "ρ"), ("omega", "ω")
  
  , ("Gamma", "Γ"), ("Xi", "Ξ"), ("Phi", "Φ"), ("Delta", "Δ"), ("Pi", "Π")
  , ("Psi", "Ψ"), ("Theta", "Θ"), ("Sigma", "Σ"), ("Omega", "Ω")
  , ("Lambda", "Λ"), ("Upsilon", "Υ") 
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
