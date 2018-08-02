{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Shell.Loop where

import Complete
import Complete.String
import Shell.CompleteHandler
import Shell.Draw
import Shell.State
import Shell.Types
import Shell.Command
import Shell.KeyPress
import Shell.CompleteHandler
import Shell.Helpers
import Shell.Update

import Control.Monad.IO.Class
import Control.Monad.RWS
import Control.Monad.RWS.Class

type FishyMonad a = forall m. (MonadState FishyState m, MonadIO m) => m a

loop :: FishyMonad Bool
loop = do
  state <- get
  nextChar <- liftIO getHiddenChar
  --cpr <- processChar' completion c


  processNextChar completion c

  return False
  --let c = getCurrentCompletion state
  --drawState' completion

  --getRebuildCompleters cpr ?-> do
    --updateCompletionHandler' 
      --(getNewCommands cpr) 
      --(getCommandLocation location)

processChar' :: CompletionHandlerResult -> Char -> FishyMonad CommandProcessResult
processChar' completerResult c = do
  state <- get
  currentDir <- liftIO $ getCurrentDirectory
  let ci = matchChar state currentDir c
  processChar (getAliases state) completerResult ci
  
