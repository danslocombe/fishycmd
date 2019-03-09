{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}

module CLI.Types where


import Complete.String
import Complete.Types
import Complete.FileCompleter
import Search

import GHC.Generics
import Data.Serialize
import Data.List.Zipper
import System.Console.ANSI
import qualified Data.Map.Lazy as Map
import Control.Monad.IO.Class
import Control.Monad.RWS.Class


type FishyMonad a = forall m. (MonadState FishyState m, MonadIO m) => m a

data Mode = Mode
  { update :: CommandInput -> FishyMonad (Maybe CLIMode)
  -- , draw :: FishyState -> IO (), should be this, drawing shouldnt modify state
  , draw :: FishyMonad ()
  }

data CLIMode = ShellMode | SearchMode

data CompletionHandler = CompletionHandler
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
  , serializedHistoryLogs    :: [String]
  } deriving (Generic, Show)

instance Serialize SerializableState 

data FishyState = FishyState 
  { getCompletionHandler     :: CompletionHandler
  , getCachedCompletions     :: CompletionHandlerResult
  , currentCompletion        :: String
  , getPrompt                :: Zipper Char
  , lastPromptHeight         :: Int -- TODO remove
  , getControlPrepped        :: Bool
  , getBufferedCommands      :: [String]
  , getCurrentDir            :: FilePath
  , getDebug                 :: Bool
  , getVerbose               :: Bool
  , getHistoryLogs           :: Zipper String
  , getHistoryIndex          :: HistoryIndex
  , getAliases               :: [Alias]
  } deriving (Show)

data Alias = Alias String [AliasElem] 
  deriving (Show, Eq)

data AliasElem = AliasStr String
               | AliasArgWildCard
               | AliasArg Int
  deriving (Show, Eq)

data CommandInput = Text (Zipper Char)
                  | Cls
                  | Complete
                  | PartialComplete
                  | Run
                  | Exit
                  | Execute String
                  | PrepControlChar
                  | HistoryBack
                  | HistoryForward
                  | Search

data CommandProcessResult = CommandProcessResult 
  { getNewCommands       :: [String]
  , getCommandLocation   :: FilePath
  , getRebuildCompleters :: Bool 
  , getExit              :: Bool
  } deriving Show
