{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module CLI.Loop where

import Complete
import Complete.String
import CLI.State
import CLI.Types
import CLI.KeyPress
import CLI.Helpers
import CLI.Modes

import Control.Monad.IO.Class
import Control.Monad.RWS
import Control.Monad.RWS.Class

loop :: Mode -> FishyMonad ()
loop mode = do
  draw mode
  loop' mode

loop' :: Mode -> FishyMonad ()
loop' mode = do

  s <- get
  nextChar <- liftIO getHiddenChar
  let commandInput = matchChar s nextChar

  next <- (update mode) commandInput

  case next of
    Just m -> do
      let mode' = lookupMode m
      draw mode'
      loop mode'
    Nothing -> return ()

  -- state <- get
  -- nextChar <- liftIO getHiddenChar

  -- processNextChar completion c

  -- return False
  --let c = getCurrentCompletion state
  --drawState' completion

  --getRebuildCompleters cpr ?-> do
    --updateCompletionHandler' 
      --(getNewCommands cpr) 
      --(getCommandLocation location)

-- processChar' :: CompletionHandlerResult -> Char -> FishyMonad CommandProcessResult
-- processChar' completerResult c = do
  -- state <- get
  -- currentDir <- liftIO $ getCurrentDirectory
  -- let ci = matchChar state currentDir c
  -- processChar (getAliases state) completerResult ci
  
