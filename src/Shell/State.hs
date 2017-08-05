{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}

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
import qualified Control.Monad.Trans.State.Strict as ST
import qualified Data.Map.Lazy as Map
import qualified Data.ByteString as BS

data FishyState = FishyState 
  { getHistoryTries          :: [StringTrie]
  , getLocalizedHistoryTries :: Map.Map FilePath [StringTrie]
  , getPathTries             :: [StringTrie]
  , getFileCompleter         :: FileCompleter
  , getPrompt                :: Zipper Char
  , lastPromptHeight         :: Int
  , getControlPrepped        :: Bool
  , getCurrentDir            :: FilePath
  , getDebug                 :: Bool
  , getVerbose               :: Bool
  , getHistoryLogs           :: Zipper String
  } deriving (Show)

data SerializableState = SerializableState
  { serializedHistoryTries   :: [StringTrie]
  , serializedLocalizedTries :: Map.Map FilePath [StringTrie]
  } deriving (Generic, Show)

instance Serialize SerializableState 

-- Run some arbitrary IO if we are running in debug mode
ifDebug :: IO () -> ST.StateT FishyState IO ()
ifDebug f = do
  state <- ST.get
  lift $ if getDebug state
    then f
    else return ()
  
-- Initialize a new 'clean' fishy state
cleanState :: Bool -> Bool -> [StringTrie] -> Map.Map FilePath [StringTrie] -> IO FishyState
cleanState debug verbose history localized = do
  FishyState history localized
    <$> genPathTries 
    <*> createFileCompleter (FileCompleter "" []) "" 
    <*> return empty 
    <*> return 0 
    <*> return False 
    <*> getCurrentDirectory 
    <*> return debug
    <*> return verbose
    <*> return empty

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
  (mapM (\(x, y) -> putStrLn(x ++ "  " ++ y)) env) >> return ()

-- Save state by serializing history tries
saveState :: FishyState -> IO ()
saveState s = do
  filepath <- statePath
  let verbose = getVerbose s
      writePath = filepath ++ stateFilename
      sstate = SerializableState (getHistoryTries s) (getLocalizedHistoryTries s)
  BS.writeFile (filepath ++ stateFilename) $ encode sstate

-- Load state by deserializing history tries
loadState :: Bool -> Bool -> IO FishyState
loadState debug verbose = do
  filepath <- statePath
  let readPath = filepath ++ stateFilename
  if verbose 
    then putStr $ "Reading state from: \"" ++ readPath ++ "\"..."
    else return ()
  exists <- doesFileExist readPath
  if exists then return ()
  else do 
    if verbose 
      then putStr "\nCreating file..."
      else return ()
    createDirectoryIfMissing True filepath
    handle <- openFile readPath ReadWriteMode
    hClose handle
    if verbose 
      then putStrLn "Done"
      else return ()
  d <- BS.readFile $ readPath
  let d' = decode d
  case d' of
    (Right (SerializableState h l)) -> do
      if verbose 
        then putStrLn "Success!"
        else return ()
      cleanState debug verbose h l
    (Left err) -> do
      if verbose 
        then putStrLn "Failed! Creating blank state"
        else return ()
      cleanState debug verbose [] Map.empty
