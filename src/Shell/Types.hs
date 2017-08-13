{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}

module Shell.Types 
  ( CompletionHandler (..)
  , SerializableState (..)
  , FishyState        (..)
  ) where

import Complete.String
import Complete.FileCompleter

import GHC.Generics
import Data.Serialize
import Data.List.Zipper
import qualified Data.Map.Lazy as Map

data CompletionHandler = CompletionHandler --CompletionHandler [FishyCompleterResult] Int
  { getHistoryTries          :: [StringTrie]
  , getLocalizedHistoryTries :: Map.Map FilePath [StringTrie]
  , getPathTries             :: [StringTrie]
  , getFileCompleter         :: FileCompleter
  , getCycle                 :: Int
  } deriving (Show)

data SerializableState = SerializableState
  { serializedHistoryTries   :: [StringTrie]
  , serializedLocalizedTries :: Map.Map FilePath [StringTrie]
  } deriving (Generic, Show)

instance Serialize SerializableState 

data FishyState = FishyState 
  { getCompletionHandler     :: CompletionHandler
  , getPrompt                :: Zipper Char
  , lastPromptHeight         :: Int -- TODO remote
  , getControlPrepped        :: Bool
  , getBufferedCommands      :: [String]
  , getCurrentDir            :: FilePath
  , getDebug                 :: Bool
  , getVerbose               :: Bool
  , getHistoryLogs           :: Zipper String
  } deriving (Show)

