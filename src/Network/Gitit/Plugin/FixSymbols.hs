-- {-# LANGUAGE #-}
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

module Network.Gitit.Plugin.FixSymbols (plugin) where

import Network.Gitit.Interface

import Data.List (isPrefixOf)

plugin :: Plugin
plugin = PageTransform $ return . processWith fixInline . processWith fixBlock

-- mkPageTransform :: Data a => (a -> a) -> Plugin
-- mkPageTransform fn = PageTransform $ return . processWith fn


fixInline :: Inline -> Inline
fixInline (Code s) = Code (codeSubst s)
fixInline x        = x

fixBlock :: Block -> Block
fixBlock (CodeBlock attr@(_,classes,_) s)
  | "haskell" `elem` classes = CodeBlock attr (codeSubst s)
fixBlock x = x

-- TODO: transform lexemes instead of strings to avoid things like "-->"
-- becoming "-→".  Use the Text.Read.Lex module in Base.  Hm.  How to
-- reconstruct white space & comments?

codeSubst :: String -> String
codeSubst = substs [ ("forall","∀"),("->","→"),(":*","×")
                   , ("\\","λ")
                   , ("`lub`","⊔"),("`glb`","⊓"), ("lub","(⊔)"),("glb","(⊓)")
                   , ("undefined","⊥"), ("bottom","⊥")
                   , ("<-","←"), ("::","∷"), ("..","‥"), ("...","⋯")
                   ]

-- TODO: Faster substitution.  Turn the from/to pairs into a single, fast
-- automaton.  I could also switch to a single-pass algorithm, instead of
-- one pass per from/to pair.

subst :: String -> String -> String -> String
subst from to = sub
 where
   sub :: String -> String
   sub "" = ""
   sub str | from `isPrefixOf` str = to ++ sub (drop n str)
   sub (c:cs) = c : sub cs
   n = length from

substs :: [(String, String)] -> String -> String
substs = foldl (.) id . map (uncurry subst) . reverse

-- The 'reverse' is to apply earlier rewrites first.  Or flip (.)