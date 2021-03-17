module Cake.Rules where

import Cake.Core
import Cake.Process
import Cake.Tex
import qualified Text.ParserCombinators.Parsek as Parsek

------------------------------------------------------
-- Patterns
extension :: String -> P (String,String)
extension s = do
  base <- Parsek.many Parsek.anySymbol
  _ <- Parsek.string s
  return (base++s,base)

anyExtension :: [String] -> P (String,String)
anyExtension ss = foldr (<|>) empty (map extension ss)

--------------------------------------------------------------
-- Rules

simple :: String -> [Char] -> (String -> [Char] -> Act ()) -> Rule
simple outExt inExt f = extension outExt <== \(output,base) ->
  let input = base ++ inExt 
  in  produce output $ do
        need input
        f output input


tex_markdown_standalone :: Rule
tex_markdown_standalone = simple ".tex" ".markdown" $ \o i -> 
  pandoc i "latex" ["--standalone"]

{-
html_markdown_standalone = simple ".html" ".markdown" $ \o i -> 
  system ["pandoc","--tab-stop=2","--standalone","-f","markdown","-t","latex", 
          "-o", o, i]
  -}
  
  
pdf_tex :: Rule
pdf_tex = simple ".pdf" ".tex" $ \o i -> 
  system ["latexmk","-pdf",i]

{-
pdf_tex_biblatex = anyExtension [".pdf",".aux"] <== \(_,c) -> 
  pdflatexBibtex c
-}

tex_lhs :: Rule
tex_lhs = extension ".tex" <== \(_,c) -> lhs2tex c

pdf_tex_biblatex :: Rule
pdf_tex_biblatex = anyExtension [".pdf",".aux"] <== \(_,c) -> 
  pdflatexBibtex c

pdf_tex_bibtex :: Rule
pdf_tex_bibtex = extension ".pdf" <== \(_,c) -> pdflatexBibtex c

allRules = tex_markdown_standalone 
--         <|> pdf_tex_bibtex
         <|> tex_lhs
