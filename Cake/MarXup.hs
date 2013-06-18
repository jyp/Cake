module Cake.MarXup where
       
import Cake.Core       
import Cake.Actions
import Cake.Process
import System.FilePath
import Cake.Tex
import Cake.Haskell

marxup x = do 
  produces [x <.> "tex",x <.> "mp"] $ do
     ghcMake x
     cut $ system $ ["./" ++ x]

pdflatexMarxup x = produce (x <.> "pdf") $ do 
  mapM_ use [x <.> "tex",x <.> "mp"]
  let delayed = x ++ "-delayed.mp"
  produce delayed $ do
    let mp = x <.> "mp"
    need mp
    cut $ do 
      remove delayed
      _mpost mp
      _mpost mp
  
  pdflatexBibtex x



pdf_marxup x = produce (x <.> "pdf") $ do 
  marxup x
  pdflatexMarxup x


