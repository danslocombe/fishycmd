{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}

module Shell.Types 
  ( CompletionHandler       (..)
  , CompletionHandlerResult (..)
  , SerializableState       (..)
  , FishyState              (..)
  , Alias                   (..)
  , AliasElem               (..)
  ) where


import Complete.String
import Complete.Types
import Complete.FileCompleter

import GHC.Generics
import Data.Serialize
import Data.List.Zipper
import System.Console.ANSI
import qualified Data.Map.Lazy as Map

data CompletionHandler = CompletionHandler --CompletionHandler [FishyCompleterResult] Int
  { getHistoryTries          :: [StringTrie]
  , getLocalizedHistoryTries :: Map.Map FilePath [StringTrie]
  , getPathTries             :: [StringTrie]
  , getFileCompleter         :: FileCompleter
  , getCycle                 :: Int
  } deriving (Show)

data CompletionHandlerResult = 
  CompletionHandlerResult [Completion Char] Color
  deriving Show

data SerializableState = SerializableState
  { serializedHistoryTries   :: [StringTrie]
  , serializedLocalizedTries :: Map.Map FilePath [StringTrie]
  } deriving (Generic, Show)

instance Serialize SerializableState 

data FishyState = FishyState 
  { getCompletionHandler     :: CompletionHandler
  , getCachedCompletions     :: CompletionHandlerResult
  , getPrompt                :: Zipper Char
  , lastPromptHeight         :: Int -- TODO remote
  , getControlPrepped        :: Bool
  , getBufferedCommands      :: [String]
  , getCurrentDir            :: FilePath
  , getDebug                 :: Bool
  , getVerbose               :: Bool
  , getHistoryLogs           :: Zipper String
  , getAliases               :: [Alias]
  } deriving (Show)

data Alias = Alias String [AliasElem] 
  deriving (Show, Eq)

data AliasElem = AliasStr String
               | AliasArgWildCard
               | AliasArg Int
  deriving (Show, Eq)
