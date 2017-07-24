{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}

module TrieState where

import Trie

import Prelude hiding (readFile, writeFile)

import GHC.Generics
import Data.Serialize
import Data.ByteString (readFile, writeFile)
import Data.Either
import Data.List.Zipper
import System.Directory
import System.Environment

data CompleteState = CompleteState 
  { getHistoryTries   :: [Trie CharWeight]
  , getFileTries      :: [Trie CharWeight]
  , getPrompt         :: Zipper Char
  , getControlPrepped :: Bool
  } deriving (Show)

stateFilename :: String
stateFilename = "trie.file"

statePath :: IO FilePath
statePath = do
  -- Debug show env
  -- env <- getEnvironment
  -- mapM (\(x, y) -> putStrLn(x ++ "  " ++ y)) env
  appdata <- getEnv "APPDATA" 
  return $ appdata ++ "\\fishycmd\\"

saveState :: CompleteState -> IO ()
saveState s = do
  filepath <- statePath
  let d = encode $ getHistoryTries s
  writeFile (filepath ++ stateFilename) $ d

loadState :: [Trie CharWeight] -> IO CompleteState
loadState fileTries = do
  filepath <- statePath
  d <- readFile $ filepath ++ stateFilename
  let d' = decode d
  complete <- return $ case d' of
    (Right decoded) -> CompleteState decoded fileTries empty False
    (Left err) -> CompleteState [] fileTries empty False
  return complete
