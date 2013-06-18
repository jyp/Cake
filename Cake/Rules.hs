module Cake.Rules where

import Cake.Core
import Cake.Process
import Cake.Actions
import Cake.Tex
import System.Directory
import System.FilePath
import Control.Applicative
import qualified Parsek
import Data.List
import Data.List.Split

------------------------------------------------------
-- Patterns
extension :: String -> P (String,String)
extension s = do
  base <- Parsek.many Parsek.anyChar
  Parsek.string s
  return (base++s,base)

anyExtension :: [String] -> P (String,String)
anyExtension ss = foldr (<|>) empty (map extension ss)

--------------------------------------------------------------
-- Rules

simple outExt inExt f = extension outExt ==> \(output,base) ->
  let input = base ++ inExt 
  in  produce output $ do
        need input
        f output input
  

tex_markdown_standalone = simple ".tex" ".markdown" $ \o i -> 
  pandoc i "latex" ["--standalone"]

{-
html_markdown_standalone = simple ".html" ".markdown" $ \o i -> 
  system ["pandoc","--tab-stop=2","--standalone","-f","markdown","-t","latex", 
          "-o", o, i]
  -}
  
  
pdf_tex = simple ".pdf" ".tex" $ \o i -> 
  system ["latexmk","-pdf",i]

{-
pdf_tex_biblatex = anyExtension [".pdf",".aux"] ==> \(_,c) -> 
  pdflatexBibtex c
-}

tex_lhs = extension ".tex" ==> \(_,c) -> lhs2tex c

allRules = tex_markdown_standalone 
--         <|> pdf_tex_bibtex
         <|> tex_lhs
