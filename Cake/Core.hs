{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, TemplateHaskell, RecordWildCards, FlexibleInstances  #-}

module Cake.Core (
  -- * Patterns and rules.
  Rule,
  P, (<==),
  
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
import Control.Monad.RWS hiding (put,get)
import qualified Control.Monad.RWS as RWS
import Control.Monad.Except
import Text.ParserCombinators.Parsek (completeResults, parse, Parser)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Binary hiding (put,get)
import System.Exit
import GHC.Generics
import System.IO

data Question = FileContents FilePath 
              | Listing FilePath String
              | Custom [String]
--              | Option [String]
              deriving (Eq, Ord, Generic)
instance Binary Question

data Failure = CakeError String | Panic | ProcessError ExitCode
             deriving (Eq, Generic)
instance Binary Failure
instance Binary ExitCode
instance Show Failure where
  show (CakeError x) = x
  show (ProcessError code) = "Process returned exit code " ++ show code
  show (Panic) = "PANIC"

data Answer = Stamp (Maybe MD5Digest) 
            | Text [String]
            | Failed Failure
            deriving (Eq, Show, Generic)
instance Binary Answer

instance Show Question where
    show (FileContents f) = "{"++f++"}"
    show (Listing f ext) = "("++f++"/*"++ext++")"
    show (Custom c) = show c

type DB = M.Map Question Answer 
type Produced = S.Set FilePath -- Set of already produced files

type P = Parser Char

-- | Rules parse names of files, and parser returns an action to build it.

type Rule = P (Act ()) 
data State = State {stateProduced :: Produced, stateStatus :: Status}
type Written = Dual DB -- take the dual so the writer overwrites old
-- entries in the DB.  Note also that we do not have a state
-- here. Answers are compared always to the previous cake run, in case
-- a question is asked more than once.
data Context = Context { ctxHandle :: Handle
                       , ctxRule :: Rule
                       , ctxDB :: DB
                       , ctxProducing :: [Question]
                       }
newtype Act a = Act (ExceptT Failure (RWST Context Written State IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState State, MonadWriter Written, MonadReader Context, MonadError Failure, MonadFail)

data Status = Clean | Dirty
            deriving (Eq,Ord)

-- | Primitve for rule construction. The given action must produce
-- files matched by the pattern.
(<==) :: P x -> (x -> Act a) -> Rule
p <== a = (\s -> a s >> return ()) <$> p

databaseFile, logFile :: String
databaseFile = ".cake"
logFile = ".cake.log"

-- | Run an action in the context of a set of rules. This is the
-- 'entry point' for user code.
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

runAct :: Rule -> DB -> Act () -> IO DB
runAct r db (Act act) = do 
  h <- openFile logFile WriteMode
  (a,Dual db') <- evalRWST (runExceptT act) (Context h r db []) (State S.empty Clean)
  case a of
     Right _ -> putStrLn "Success!"
     Left e -> putStrLn $ "cake: " ++ show e
  hClose h
  return db'


-- | Was the file already produced /in this run/?
produced :: FilePath -> Act Bool
produced f = do
  ps <- RWS.gets stateProduced
  return $ f `S.member` ps

-- | record the current question being answered
modCx :: Question -> Context -> Context
modCx q (Context {..}) = Context {ctxProducing = q:ctxProducing,..}

-- | Answer a question using the action given.
-- The action is encapsulated. That is, clobbering done by the action
-- is hidden.  However, if the answer to the question turns out
-- different from the recorder answer, then clobbering is flagged.
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
-- The result is /not/ compared to the previous run, so it is the
-- caller's responsibility that the new answer is properly taken into
-- account.
refresh :: Question -> Act Answer -> Act Answer
refresh q act =
  do a <- act
     tell (Dual $ M.singleton q a)
     return a
  `catchError` \ e -> do -- on error
     tell (Dual $ M.singleton q $ Failed e) -- Answering the question failed...
     throwError e -- and questions depending on it will also fail

-- | Produce a file, using the given action.
produce :: FilePath -> Act () -> Act ()
produce x = produces [x]

-- | Produce files, using the given action.
produces :: [FilePath] -> Act () -> Act ()
produces fs a = do
  ps <- mapM produced fs -- Do nothing if all files are already produced.
  when (not $ and ps) $ updates fs a

-- | Produce a file, using with the given action.  BUT: no problem to
-- produce the same file multiple times.
updates :: [FilePath] -> Act () -> Act ()
updates [] a = a
updates (f:fs) a = distill (FileContents f) (do
          e <- liftIO $ doesFileExist f
          updates fs (when (not e) clobber >> a) -- force running the action if the file is not present, even if in a clean state.
          modify $ \State{..} -> State {stateProduced = S.insert f stateProduced,..}  -- remember that the file has been produced already
          fileStamp f) >> return ()


-- | List directory contents by extension
list :: FilePath -> String -> Act [String]
list directory extension = do
  Text x <- distill (Listing directory extension) $ do
    files <- filter (filt . takeExtension) <$> liftIO (getDirectoryContents directory)
    return $ Text (map (directory </>) files)
  return x
 where filt = if null extension then const True else (== '.':extension)


-- | Mark that a file is used. Do not chase dependencies on this file
-- though. (To be used eg. if a command uses an optional file).
use :: FilePath -> Act Answer
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
  State ps status <- RWS.get
  RWS.put (State ps Clean)
  x <- a
  RWS.modify $ \State{..} -> State {stateStatus=status,..}
  return x

-- | Run the action, but do not clobber the state.
noClobber :: Act a -> Act a
noClobber a = do
  s <- RWS.gets stateStatus
  x <- a
  RWS.modify $ \State{..} -> State {stateStatus=s,..}
  return x

independently :: [Act a] -> Act ()  
independently as = do
  s <- RWS.gets stateStatus
  ds <- forM as $ \a -> do
    RWS.modify $ \State{..} -> State {stateStatus=s,..}
    _ <- a  
    RWS.gets stateStatus
  RWS.modify   $ \State{..} -> State {stateStatus=(maximum $ s:ds),..}
    

    

findRule :: FilePath -> Act (Maybe (Act ()))
findRule f = do
  r <- ctxRule <$> ask
  let rs = parse r completeResults f
  case rs of
    Right [x] -> return (Just x)
    Right _ -> throwError $ CakeError $ "More than one rule for file " ++ f
    Left _e -> do 
      debug $ "No rule for file: " ++ f
      -- debug $ "Parser says: " ++ show e
      return Nothing
     
debug :: String -> Act ()           
debug x = do 
  h <- ctxHandle <$> ask
  ps <- ctxProducing <$> ask
  s <- RWS.gets stateStatus
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

clobber :: Act ()
clobber = RWS.modify $ \State{..} -> State {stateStatus=Dirty,..}

-- | Run the action in only in a clobbered state
cut :: Act () -> Act ()
cut x = do
  s <- RWS.gets stateStatus
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
      _ <- use f
      return ()
    Just a -> a

needs :: [FilePath] -> Act ()
needs = independently . map need


  
