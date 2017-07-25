{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}

module TrieState where

import Trie
import FileTries

import Prelude hiding (readFile, writeFile)

import GHC.Generics
import Data.Serialize
import Data.ByteString (readFile, writeFile)
import Data.Either
import Data.List.Zipper
import System.Directory
import System.Environment
import Data.List.Split
import Control.Monad
import qualified Control.Monad.Trans.State.Strict as ST
import Control.Monad.Trans.Class

data CompleteState = CompleteState 
  { getHistoryTries   :: [Trie CharWeight]
  , getFileTries      :: [Trie CharWeight]
  , getPathTries      :: [Trie CharWeight]
  , getPrompt         :: Zipper Char
  , getControlPrepped :: Bool
  , getCurrentDir     :: FilePath
  , getDebug             :: Bool
  } deriving (Show)

ifDebug :: IO () -> ST.StateT CompleteState IO ()
ifDebug f = do
  state <- ST.get
  lift $ if getDebug state
  then f
  else return ()
  
cleanState :: Bool -> [Trie CharWeight] -> [Trie CharWeight] -> IO CompleteState
cleanState debug history files = do
  CompleteState history files <$> genPathTries <*> return empty <*> return False <*> getCurrentDirectory <*> return debug

genPathTries :: IO [Trie CharWeight]
genPathTries = do
  path <- getEnv "PATH"
  let pathSplit = splitOn ";" path
  validPaths <- filterM doesPathExist pathSplit
  files <- mapM listDirectory validPaths
  return $ buildTries $ concat files
  
stateFilename :: String
stateFilename = "trie.file"

debugPrintEnvironment :: IO ()
debugPrintEnvironment = do
  env <- getEnvironment
  (mapM (\(x, y) -> putStrLn(x ++ "  " ++ y)) env) >> return ()

statePath :: IO FilePath
statePath = do
  appdata <- getEnv "APPDATA" 
  return $ appdata ++ "\\fishycmd\\"

saveState :: CompleteState -> IO ()
saveState s = do
  filepath <- statePath
  let d = encode $ getHistoryTries s
  writeFile (filepath ++ stateFilename) $ d

loadState :: Bool -> [Trie CharWeight] -> IO CompleteState
loadState debug fileTries = do
  filepath <- statePath
  d <- readFile $ filepath ++ stateFilename
  let d' = decode d
  complete <- case d' of
    (Right decoded) -> cleanState debug decoded fileTries
    (Left err) -> cleanState debug [] fileTries
  return complete
