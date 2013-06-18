module Cake.Haskell where
       
import Cake.Core       
import Cake.Actions
import Cake.Process
import System.FilePath
import Data.List

importedHaskell = map (moduleToFile . dropSpaces . drop (length prefix)) . filter (prefix `isPrefixOf`) . map dropSpaces . lines
  where prefix = "import"
        moduleToFile m = map dotToSlash m <.> "hs"
        dotToSlash '.' = '/'
        dotToSlash c = c


ghcMake x = produce x $ do
   let src = x <.> "hs"
   chaseDeps importedHaskell src
   system ["ghc", "-o", x, "--make", src]
              
