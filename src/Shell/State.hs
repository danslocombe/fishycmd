module Shell.State 
    ( FishyState(..)
    , loadState
    , saveState
    , ifDebug
    , cleanState
    , statePath
    ) where

import Complete.String
import Complete.FileCompleter
import Shell.Types
import Shell.Helpers
import Corext.AliasCompleter

import Prelude

import GHC.Generics
import Data.Serialize
import Data.Either
import Data.Maybe (listToMaybe, catMaybes)
import Data.List.Zipper
import System.Directory
import System.Environment
import System.IO
import Data.List.Split
import Control.Monad
import Control.Monad.Trans.Class
import Text.Regex.Posix ((=~))
import System.Console.ANSI
import qualified Control.Monad.Trans.State.Strict as ST
import qualified Data.Map.Lazy as Map
import qualified Data.ByteString as BS
  
-- Initialize a new 'clean' fishy state
cleanState :: Bool -> Bool -> [StringTrie] -> Map.Map FilePath [StringTrie] -> IO FishyState
cleanState debug verbose global local = do
  handler <- (CompletionHandler global local
    <$> genPathyTries 
    <*> createFileCompleter (FileCompleter "" []) ""
    <*> return 0)
  FishyState
    <$> return handler
    <*> return (CompletionHandlerResult [] Red)
    <*> return empty 
    <*> return 0 
    <*> return False 
    <*> return []
    <*> getCurrentDirectory 
    <*> return debug
    <*> return verbose
    <*> return empty

genPathyTries :: IO [StringTrie]
genPathyTries = inCorext >>= \x -> if x
  then genCorextTries
  else genPathTries

genCorextTries :: IO [StringTrie]
genCorextTries = putStrLn "Using CoreXT for completions" >>     
                 buildTries <$> parseAliases

-- TODO do this concurrently
genPathTries :: IO [StringTrie]
genPathTries = do
  path <- getEnv "PATH"
  let pathSplit = splitOn ";" path
  validPaths <- filterM doesPathExist pathSplit
  files <- concat <$> mapM listDirectory validPaths
  let removeExe s = take (length s - 4) s
      files' = map removeExe $ filter (\x -> x =~ "(.+)\\.exe$") files
  return $ buildTries $ files'
  
stateFilename :: String
stateFilename = "trie.file"

statePath :: IO FilePath
statePath = do
  appdata <- getEnv "APPDATA" 
  return $ appdata ++ "\\fishycmd\\"

-- Unused
printEnvironment :: IO ()
printEnvironment = do
  env <- getEnvironment
  (mapM_ (\(x, y) -> putStrLn(x ++ "  " ++ y)) env)

serializableFromFishy :: FishyState -> SerializableState
serializableFromFishy fs =
  let ch = getCompletionHandler fs
  in SerializableState 
    (getHistoryTries ch) 
    (getLocalizedHistoryTries ch)

-- Save state by serializing history tries
saveState :: FishyState -> IO ()
saveState s = do
  filepath <- statePath
  let verbose = getVerbose s
      writePath = filepath ++ stateFilename
      sstate = serializableFromFishy s
  BS.writeFile (filepath ++ stateFilename) $ encode sstate

-- Load state by deserializing history tries
loadState :: Bool -> Bool -> IO FishyState
loadState debug verbose = do
  filepath <- statePath
  let readPath = filepath ++ stateFilename

  verbose ?->
    (putStr $ "Reading state from: \"" ++ readPath ++ "\"...")

  not <$> doesFileExist readPath
    ?~> do
      verbose ?-> putStr "\nCreating file..."
      createDirectoryIfMissing True filepath
      handle <- openFile readPath ReadWriteMode
      hClose handle
      verbose ?-> putStrLn "Done"

  d <- BS.readFile $ readPath
  let d' = decode d
  case d' of
    (Right (SerializableState h l)) -> do
      verbose ?-> putStrLn "Success!"
      cleanState debug verbose h l
    (Left err) -> do
      verbose ?-> putStrLn "Failed! Creating blank state"
      cleanState debug verbose [] Map.empty
