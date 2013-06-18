{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, TemplateHaskell, RecordWildCards, FlexibleInstances  #-}

module Cake.Core (
  -- * Patterns and rules.
  Rule,
  P, (==>),
  
  -- * High-level interface
  Act,             
  cake,
  need, needs,           
  list,
  -- * Mid-level interface
  produce, produces,
  cut, independently,
  -- * Low-level interface
  debug,
  distill,
  fileStamp,
  shielded,
  use,
  updates,
  Question(..),
  Answer(..),
  Failure(..),
  -- * Re-exports
  module Control.Applicative,
  throwError,
  ) where

import Data.Digest.Pure.MD5

import qualified Data.ByteString.Lazy as B
import System.Directory
import System.FilePath
import Control.Applicative
import Control.Monad (when)
import Control.Monad.RWS hiding (put,get)
import qualified Control.Monad.RWS as RWS
import Control.Monad.Error 
import qualified Parsek
import Parsek (completeResults, parse, Parser)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Binary hiding (put,get)
import System.Exit
import Control.Arrow (second,first)

import Data.DeriveTH
import Data.Binary
import System.IO

data Question = FileContents FilePath 
              | Listing FilePath String
              | Custom [String]
--              | Option [String]
              deriving (Eq, Ord)
$( derive makeBinary ''Question )             
                         
data Failure = CakeError String | Panic | ProcessError ExitCode
             deriving (Eq)
instance Show Failure where
  show (CakeError x) = x
  show (ProcessError code) = "Process returned exit code " ++ show code
  show (Panic) = "PANIC"
$( derive makeBinary ''ExitCode )                                  
$( derive makeBinary ''Failure )             

data Answer = Stamp (Maybe MD5Digest) 
            | Text [String]
            | Failed Failure
            deriving (Eq, Show)
$( derive makeBinary ''Answer )             

instance Show Question where
    show (FileContents f) = "{"++f++"}"
    show (Listing f ext) = "("++f++"/*"++ext++")"
    show (Custom c) = show c

type DB = M.Map Question Answer
type Produced = S.Set FilePath      

type P = Parser Char

-- | Rules map names of files to actions building them.
type Rule = P (Act ()) 
type State = (Produced,Status)
type Written = Dual DB -- take the dual so the writer overwrites old entries in the DB.
data Context = Context {ctxHandle :: Handle, ctxRule :: Rule, ctxDB :: DB, ctxProducing :: [Question]}
newtype Act a = Act (ErrorT Failure (RWST Context Written State IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState State, MonadWriter Written, MonadReader Context, MonadError Failure)

data Status = Clean | Dirty 
            deriving (Eq,Ord)

instance Error Failure where
   noMsg = Panic
   strMsg = CakeError                  

instance Applicative P where
  (<*>) = ap
  pure = return

instance Alternative P where
  (<|>) = (Parsek.<|>)
  empty = Parsek.pzero
                     
-- | Primitve for rule construction. The given action must produce
-- files matched by the pattern.
(==>) :: P x -> (x -> Act a) -> Rule
p ==> a = (\s -> do a s;return ()) <$> p

databaseFile = ".cake"
logFile = ".cake.log"

-- | Run an action in the context of a set of rules.
cake :: Rule -> Act () -> IO ()
cake rule action = do
  e <- doesFileExist databaseFile 
  oldDB <- if e 
    then decodeFile databaseFile
    else return $ M.empty
  newAnswers <- runAct rule oldDB action
  let newDB = newAnswers <> oldDB -- new answers overwrite old ones
  putStrLn $ "Database is:"
  forM_ (M.assocs newDB) $ \(k,v) ->
    putStrLn $ (show k) ++ " => " ++ (show v)
  encodeFile databaseFile newDB

-- | Produce a shell script rebuilding everything
-- eatIt :: Rule -> Act () -> String ()  
  
-- | Was the file already produced?
produced :: FilePath -> Act Bool
produced f = do 
  (ps,_) <- RWS.get
  return $ f `S.member` ps

modCx q (Context {..}) = Context {ctxProducing = q:ctxProducing,..}    

-- | Answer a question using the action given. The action is
-- encapsulated by the answer. That is, clobbering done by the action
-- will be hidden if the answer to the question turns out the same as
-- in a previous run.
distill :: Question -> Act Answer -> Act Answer
distill q act = local (modCx q) $ do
  debug $ "Starting to answer: " ++ show q
  db <- ctxDB <$> ask
  a1 <- refresh q $ noClobber act
  let same = Just a1 == M.lookup q db
  debug $ "Old answer: " ++ show (M.lookup q db)
  debug $ "New answer: " ++ show a1
  when (not same) clobber
  debug $ "Same? "  ++ show same
  return a1

-- | Answer a question using the action given. 
-- The result is not compared to the
-- previous run, so it is the caller responsibility that the new
-- answer is properly taken into account.
refresh :: Question -> Act Answer -> Act Answer
refresh q act = 
  do a <- act
     tell (Dual $ M.singleton q a)
     return a
  `catchError` \ e -> do -- on error
     tell (Dual $ M.singleton q $ Failed e) -- Answering the question failed...
     throwError e -- and questions depending on it will also fail

produce x = produces [x]

-- | Produce a file, using the given action.
produces :: [FilePath] -> Act () -> Act ()
produces fs a = do
  ps <- mapM produced fs -- Do nothing if the file is already produced.
  when (not $ and ps) $ updates fs a

-- | Produce a file, using with the given action.  BUT: no problem to
-- produce the same file multiple times.  
updates :: [FilePath] -> Act () -> Act ()
updates [] a = a 
updates (f:fs) a = distill (FileContents f) (do
          e <- liftIO $ doesFileExist f
          updates fs (when (not e) clobber >> a) -- force running the action if the file is not present, even if in a clean state.
          modify $ first $ S.insert f -- remember that the file has been produced already
          fileStamp f) >> return ()


-- | List directory contents by extension
list directory extension = do 
  Text x <- distill (Listing directory extension) $ do
   files <- filter (filt . takeExtension) <$> liftIO (getDirectoryContents directory)
   return $ Text (map (directory </>) files)
  return x
 where filt = if null extension then const True else (== '.':extension)


-- | Mark that a file is used. Do not chase dependencies on this file
-- though. (To be used eg. if a command uses an optional file).
use f = distill (FileContents f) (fileStamp f)


-- | File was modified by some command, but in a way that does not
-- invalidate previous computations. (This is probably only useful for
-- latex processing).
overwrote :: FilePath -> Act Answer
overwrote f = refresh (FileContents f) (fileStamp f)

-- | Run the argument in a clean context, and do not clobber the state
-- even if the argument does that.  To use when the construction of
-- the argument actually does not depend on the previous questions
-- asked, and the constructed thing is "atomic" for the environment.

-- NOTE: This can be used only when the purpose of the argument (why
-- we call it) is known -- for example we already have determined that
-- another goal depends on what we're going to perform. The dirty flag
-- must be set independently in the context if the produced object is
-- not present.
shielded :: Act a -> Act a
shielded a = do
  (ps,s) <- RWS.get
  RWS.put (ps,Clean)
  x <- a
  (ps',_) <- RWS.get
  RWS.modify (second (const s))
  return x

-- | Run the action, but do not clobber the state.
noClobber :: Act a -> Act a  
noClobber a = do
  s <- snd <$> RWS.get
  x <- a
  RWS.modify (second (const s))
  return x

independently :: [Act a] -> Act ()  
independently as = do
  (ps,s) <- RWS.get
  ds <- forM as $ \a -> do
    RWS.modify (second (const s))
    a  
    snd <$> RWS.get
  RWS.modify (second (const (maximum $ s:ds)))
    

    
runAct :: Rule -> DB -> Act () -> IO DB
runAct r db (Act act) = do 
  h <- openFile logFile WriteMode
  (a,Dual db) <- evalRWST (runErrorT act) (Context h r db []) (S.empty,Clean)
  case a of
     Right _ -> putStrLn "Success!"
     Left e -> putStrLn $ "cake: " ++ show e
  hClose h
  return db    

findRule :: FilePath -> Act (Maybe (Act ()))
findRule f = do
  r <- ctxRule <$> ask
  let rs = parse r completeResults f
  case rs of
    Right [x] -> return (Just x)
    Right _ -> throwError $ CakeError $ "More than one rule for file " ++ f
    Left e -> do 
      debug $ "No rule for file: " ++ f
      -- debug $ "Parser says: " ++ show e
      return Nothing
     
debug :: String -> Act ()           
debug x = do 
  h <- ctxHandle <$> ask
  ps <- ctxProducing <$> ask
  (_,s) <- RWS.get
  let st = case s of
              Clean -> "O"
              Dirty -> "X"
  liftIO $ hPutStrLn h $ st ++ " "++ concat (map (++": ") $ reverse $ map show ps) ++ x 

-- | Return a stamp (hash) for a file
fileStamp :: FilePath -> Act Answer  
fileStamp f = liftIO $ do
  e <- doesFileExist f
  Stamp <$> if e 
    then Just <$> md5 <$> B.readFile f
    else return Nothing

clobber = RWS.modify $ second $ (const Dirty)

-- | Run the action in only in a clobbered state
cut :: Act () -> Act ()
cut x = do
  (_,s) <- RWS.get
  case s of
    Clean -> debug $ "Clean state; skipping."
    Dirty -> x

-- | Try to build a file using known rules; then mark it as used.
need :: FilePath -> Act ()
need f = do
  debug $ "Need: " ++ f
  r <- findRule f
  case r of      
    Nothing -> do 
      e <- liftIO $ doesFileExist f
      when (not e) $ throwError $ CakeError $ "No rule to create " ++ f
      debug $ "using existing file"
      use f
      return ()
    Just a -> a

needs = independently . map need


  
