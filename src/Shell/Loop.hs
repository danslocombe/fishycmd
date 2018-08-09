{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Shell.Loop where

import Complete
import Complete.String
import Shell.CompleteHandler
import Shell.State
import Shell.Types
import Shell.KeyPress
import Shell.Helpers

import Control.Monad.IO.Class
import Control.Monad.RWS
import Control.Monad.RWS.Class

loop :: Mode -> FishyMonad ()
loop mode = do
  draw mode
  loop' mode

loop' :: Mode -> FishyMonad ()
loop' Mode{..} = do

  s <- get
  nextChar <- liftIO getHiddenChar
  let commandInput = matchChar s nextChar

  next <- update commandInput

  draw

  case next of
    Just m -> loop m
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
  
