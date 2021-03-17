{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}
import System.Process
import System.Console.CmdArgs.Implicit
import Data.List (intercalate)
-- import Paths_cake
-- import Data.Version (Version(..))
import Text.Regex.TDFA
import Data.Array
import System.FilePath
import System.Directory

data Args = Args {targets' :: [String],
                  rules :: String,
                  cakefile :: String
                 } 
  deriving (Show, Data, Typeable)


opts = cmdArgsMode $ Args { 
  rules = "rules" &= help "rules to use" &= typ "Rule",
  cakefile = "Cakefile.hs" &= name "f" &= help "cakefile to use" &= typFile,
  targets' = [] &= args &= typ "Act ()"
  } &= program "cake" 
--    &= summary ("cake " ++ intercalate "." (map show (versionBranch version)))

paren x = "(" ++ x ++ ")"         

regex :: Regex
regex = makeRegex "import[ \t]+(qualified[ \t]+)?([^ \t]+)[ \t]*--[ \t]*FROM[ \t]*([^ \t]+)[ \t]*"

define :: Show a => ([Char], a) -> [Char]
define (modul,dir) = "-DROOT_" ++ modul ++ "=" ++ show (show dir)

mainCake Args{..} = do  
  cf <- readFile cakefile
  
  let 
    targets = (if null targets' then ("action":) else id) targets'
    imports = [(fst (arr!2), addTrailingPathSeparator $ fst (arr!3)) | l <- lines cf, Just (_,arr,_) <- [matchOnceText regex l]] 
    defines = intercalate " " $ map define imports
    searchdir = "-i" ++ intercalate ":" (map snd imports)
    expr = "cake " ++ paren rules ++ paren (intercalate " >> " $ map paren targets)
    command = "ghc -XCPP " ++ searchdir ++ " " ++ defines ++ " -e " ++ show expr ++ " " ++ cakefile 
  putStrLn $ "imports: " ++ show imports
  putStrLn $ "cake: running " ++ command
  system command


main = do
  Args {..} <- cmdArgsRun opts
  e <- doesFileExist cakefile
  (if e then mainCake else error "Cakefile.hs not found") Args{..}
  return ()


-- Local Variables:
-- dante-target: "exe:cake"
-- End:
