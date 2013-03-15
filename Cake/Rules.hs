module Cake.Rules where

import Cake.Core
import Cake.Process
import System.Directory
import System.FilePath
import Control.Applicative
import Control.Monad (when)
import Control.Monad.RWS (liftIO)
import qualified Parsek
import Parsek (completeResults, parse, Parser)
import Data.List
import Data.List.Split
import Data.Char (isSpace)

------------------------------------------------------
-- Patterns
extension :: String -> P (String,String)
extension s = do
  base <- Parsek.many Parsek.anyChar
  Parsek.string s
  return (base++s,base)

anyExtension :: [String] -> P (String,String)
anyExtension ss = foldr (<|>) empty (map extension ss)

---------------------------------------------------------
-- Actions

copy :: FilePath -> FilePath -> Act()
copy from to = shielded $ produce to $ needing [from] $ do
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
 
graphviz program inp typ options = produce out $ needing [inp] $ do
  system $ [program, "-T"++typ, "-o"++out, inp] ++ options
 where out = replaceExtension inp typ
  
{-
mpostDeriv = extension "-delayed.mp" ==> \s -> do
  let input = s ++ ".mp"
  need input
  rm (s ++ "-delayed.mp")
  mpost input
  mpost input
   -}

needing :: [FilePath] -> Act () -> Act ()
needing xs act = do
  mapM_ need xs
  cut act
          
--------------------------------------------------------------
-- Rules

simple outExt inExt f = extension outExt ==> \(output,base) ->
  let input = base ++ inExt 
  in  produce output $ needing [input]  $ f output input
  

tex_markdown_standalone = simple ".tex" ".markdown" $ \o i -> 
  pandoc i "latex" ["--standalone"]

{-
html_markdown_standalone = simple ".html" ".markdown" $ \o i -> 
  system ["pandoc","--tab-stop=2","--standalone","-f","markdown","-t","latex", 
          "-o", o, i]
  -}
  
  
pdf_tex = simple ".pdf" ".tex" $ \o i -> 
  system ["latexmk","-pdf",i]

argsOf s = map (init . (drop $ 2 + length s)) . filter (("\\" ++ s ++ "{") `isPrefixOf`) 

getBibFiles input = distill (Custom ["bibfiles",input]) $ do
    ls <- map (drop 14) . filter ("\\bibliography{" `isPrefixOf`) . lines <$> Cake.Rules.readFile input
    let bibs = map (++".bib") $ case ls of
            [] -> []
            (l:_) -> splitOn "," $ reverse . dropWhile (== '}') . reverse $ l
    return $ Text bibs

s ++? e = if e `isSuffixOf` s then s else s ++ e

dropSpaces = reverse . dropWhile isSpace . reverse . dropWhile isSpace 

includedTex input = map (++? ".tex") . argsOf "input" . lines <$> Cake.Rules.readFile input

includedLhs input = filter (`notElem` stdinclude) . map (dropSpaces . drop 8) . filter ("%include" `isPrefixOf`) . map (dropWhile isSpace). lines <$> Cake.Rules.readFile input
  where stdinclude = ["lhs2TeX.fmt","polycode.fmt"]

chaseDeps includedFiles i = do
  is <- includedFiles i
  mapM_ (chaseDeps includedFiles) is
  


pdflatexBibtex c = do
  let input = c ++ ".tex"
      aux = c ++ ".aux"
      pdf = c ++ ".pdf"
      bbl = c ++ ".bbl"
  chaseDeps includedTex input
  produce aux $ cut $ _pdflatex c
  
  shielded $ do
    use aux
    Text bibs <- getBibFiles input  
    mapM_ need bibs      
    produce bbl $ cut $_bibtex c
    
  use bbl
  produce pdf $ cut $ do 
    _pdflatex c
    overwrote aux
    return ()
  

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


lhs2tex c = do
  let lhs = c ++ ".lhs"
      tex = c ++ ".tex"
  chaseDeps includedLhs lhs
  produce tex $ _lhs2TeX lhs tex
             

tex_lhs = extension ".tex" ==> \(_,c) -> lhs2tex c

allRules = tex_markdown_standalone 
--         <|> pdf_tex_bibtex
         <|> tex_lhs
