module Cake.Process (processIO, Cake.Process.system) where

import Cake.Core
import System.IO
import Control.Applicative
import Control.Monad.Trans
import System.Process as P
import Data.List
import System.Exit
import Control.Monad

quote :: String -> String
quote = show

maybeM :: Applicative m => (a -> m Handle) -> Maybe a -> m StdStream
maybeM f Nothing = pure Inherit
maybeM f (Just x) = UseHandle <$> f x

processIO' :: FilePath -> [String] -> Maybe FilePath -> Maybe FilePath -> Act ExitCode
processIO' cmd args inp out = do
  debug $ "running: "++ cmd ++ " " ++ intercalate " " args
  liftIO $ do
    o <- maybeM (flip openFile WriteMode) out
    i <- maybeM (flip openFile ReadMode) inp
    (_,_,_,p) <- createProcess $ (shell (intercalate " " $ map quote (cmd:args))) {std_in = i, std_out = o}
    waitForProcess p
    
    
system' :: [String] -> Act ExitCode
system' (x:xs) = processIO' x xs Nothing Nothing

succeed :: Act ExitCode -> Act ()
succeed act = do
  code <- act
  when (code /= ExitSuccess) $ throwError $ ProcessError code

system xs = succeed $ system' xs
processIO c a i o = succeed $ processIO' c a i o