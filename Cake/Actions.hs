module Cake.Actions where

import Cake.Core
import Cake.Process
import System.Directory
import System.FilePath
import Control.Applicative
import Control.Monad (when)
import Control.Monad.RWS (liftIO)
import Data.List
import Data.List.Split
import Data.Char (isSpace)

copy :: FilePath -> FilePath -> Act()
copy from to = shielded $ produce to $ do
  needs [from]
  cut $ do
    mkdir $ takeDirectory to
    liftIO $ copyFile from to

mkdir :: FilePath -> Act ()    
mkdir d = liftIO $ createDirectoryIfMissing True d
    
touch :: FilePath -> Act ()
touch x = shielded $ produce x $ do
  system ["touch",x]
  
readFile :: FilePath -> Act String
readFile x = do
  need x
  liftIO $ Prelude.readFile x
  
_pdflatex x = system ["pdflatex",x]
  
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

s ++? e = if e `isSuffixOf` s then s else s ++ e

dropSpaces = reverse . dropWhile isSpace . reverse . dropWhile isSpace 

includedTex input = map (++? ".tex") . argsOf "input" . lines <$> Cake.Actions.readFile input

includedLhs input = filter (`notElem` stdinclude) . map (dropSpaces . drop 8) . filter ("%include" `isPrefixOf`) . map (dropWhile isSpace). lines <$> Cake.Actions.readFile input
  where stdinclude = ["lhs2TeX.fmt","polycode.fmt"]

chaseDeps includedFiles i = do
  is <- includedFiles i
  mapM_ (chaseDeps includedFiles) is
  
pdflatexBibtex c = do
  let input = c ++ ".tex"
      aux = c ++ ".aux"
      pdf = c ++ ".pdf"
      bbl = c ++ ".bbl"

  produces [aux,pdf] $ do
    chaseDeps includedTex input
    cut $ _pdflatex c
  
  produce bbl $ do
    Text bibs <- getBibFiles input  
    needs bibs      
    cut $ do 
      _bibtex c    
      updates [aux,pdf] $ _pdflatex c
  

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

