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
copy from to = produce to $ do
  needs [from]
  cut $ do
    mkdir $ takeDirectory to
    liftIO $ copyFile from to

mkdir :: FilePath -> Act ()    
mkdir d = liftIO $ createDirectoryIfMissing True d
    
touch :: FilePath -> Act ()
touch x = produce x $ do
  system ["touch",x]
  
remove x = system ["rm", "-f", x]
    
readFile :: FilePath -> Act String
readFile x = do
  need x
  liftIO $ Prelude.readFile x
  
dropSpaces = reverse . dropWhile isSpace . reverse . dropWhile isSpace 

-- | Depend on the input file and its dependencies, as given by the 1st argument.
chaseDeps includedFiles i = do
  is <- includedFiles <$> Cake.Actions.readFile i
  mapM_ (chaseDeps includedFiles) is

-- | Depend on the input file and its dependencies. If a file cannot
-- be found, do not depend on it.
chaseSoftDeps includedFiles i = do
  e <- liftIO $ doesFileExist i
  when e $ do 
    is <- includedFiles <$> Cake.Actions.readFile i
    mapM_ (chaseDeps includedFiles) is


s ++? e = if e `isSuffixOf` s then s else s ++ e


