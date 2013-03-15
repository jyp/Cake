module Cake.Marxup where
       
import Cake.Core       
import Cake.Actions
import Cake.Process
import System.FilePath

import Parsek

ghcMake x = produce x $ do
              let src = x <.> "hs"
              need src
              system ["ghc", "-o", x, "--make", src]
              

marxup x = do 
  produce (x <.> "tex") $ do
     ghcMake x
     cut $ 
       system $ ["./" ++ x]
  produce (x <.> "mp") $ return ()

mpost x = system ["mpost", x]

remove x = system ["rm", "-f", x]

pdf_marxup x = produce (x <.> "pdf") $ do 
  marxup x
  let delayed = x ++ "-delayed.mp"
  produce delayed $ do
    let mp = x <.> "mp"
    need mp
    cut $ do 
      remove delayed
      mpost mp
      mpost mp
  
  cut $ _pdflatex x


