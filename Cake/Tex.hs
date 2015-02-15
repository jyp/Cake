module Cake.Tex where

import Cake.Core
import Cake.Process
import System.Directory
import System.FilePath
import Control.Applicative
import Control.Monad.RWS (liftIO)
import Control.Monad (when)
import Data.List
import Data.List.Split
import Data.Char (isSpace)
import Cake.Actions

_mpost x = system ["mpost", x]

_pdflatex x = system ["pdflatex",x]
_xelatex x = system ["xelatex",x]
  
_bibtex x = system ["bibtex",x]


pandoc inp typ options = produce out $ do 
  need inp
  cut $ system $ ["pandoc",inp,"-t",typ,"-o",out] ++ options
 where out = replaceExtension inp ext
       ext = case typ of 
               "latex" -> "tex"
               _ -> typ

graphviz program inp typ options = produce out $ do
  needs [inp]          
  cut $ system $ [program, "-T"++typ, "-o"++out, inp] ++ options
 where out = replaceExtension inp typ
  
-- | Argument of a latex macro
argsOf s = map (init . (drop $ 2 + length s)) . filter (("\\" ++ s ++ "{") `isPrefixOf`) 

-- | Find the bib files used in a tex file
getBibFiles input = distill (Custom ["bibfiles",input]) $ do
    ls <- map (drop 14) . filter ("\\bibliography{" `isPrefixOf`) . lines <$> Cake.Actions.readFile input
    let bibs = map (++".bib") $ case ls of
            [] -> []
            (l:_) -> splitOn "," $ reverse . dropWhile (== '}') . reverse $ l
    return $ Text bibs

includedTex = map (++? ".tex") . argsOf "input" . lines

includedLhs = filter (`notElem` stdinclude) . 
              map (dropSpaces . drop (length prefix)) . filter (prefix `isPrefixOf`) . 
              map (dropWhile isSpace) . 
              lines
  where stdinclude = ["lhs2TeX.fmt","polycode.fmt"]
        prefix = "%include"

latexBibtex engine c = do
  let [input,aux,pdf,bbl] = map (c <.>) ["tex","aux","pdf","bbl"]

  produces [aux,pdf] $ do
    chaseDeps includedTex input
    cut $ engine c

  bibliographyExists <- do
    aux_contents <- Cake.Actions.readFile aux
    return ("\\bibdata" `isInfixOf` aux_contents)

  when bibliographyExists $ do
    produce bbl $ do
      Text bibs <- getBibFiles input
      needs bibs
      cut $ _bibtex c

  updates [aux,pdf] $ do
    use aux
    when bibliographyExists $ use bbl >> return ()
    cut $ engine c

pdflatexBibtex = latexBibtex _pdflatex
xelatexBibtex = latexBibtex _xelatex

{-
pdf_tex_bibtex = extension ".pdf" ==> \(_,c) -> pdflatexBibtex c

pdflatexBiblatex c = do
  let input = c ++ ".tex"
      aux = c ++ ".aux"
      pdf = c ++ ".pdf"
  produce pdf $ do
    produce aux $ do 
      chaseDeps includedTex input
      cut $ _pdflatex c
    
    produce (c ++ ".bbl") $ do
      -- Note that this does not depend on the actual tex file; only the list of bibs.
      Text bibs <- getBibFiles input
      mapM_ need bibs
      use aux
      when (not $ null bibs) $
        cut $ _bibtex c

    cut $ do _pdflatex c
             overwrote aux
             return ()
    

pdf_tex_biblatex = anyExtension [".pdf",".aux"] ==> \(_,c) -> 
  pdflatexBibtex c

-}
_lhs2TeX i o = do
  system ["lhs2TeX","-o",o,i]


lhs2tex c = produce tex $ do
  chaseDeps includedLhs lhs
  cut $ _lhs2TeX lhs tex
 where lhs = c ++ ".lhs"
       tex = c ++ ".tex"
