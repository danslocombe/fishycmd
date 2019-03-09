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

import Data.Char (chr, ord)

loop :: Mode -> FishyMonad ()
loop mode = do
  draw mode
  loop' mode

loop' :: Mode -> FishyMonad ()
loop' mode = do

  s <- get
  nextChar <- liftIO getHiddenChar
  let commandInput = matchChar s nextChar
  liftIO $ putStrLn $ show $ ord nextChar

  next <- (update mode) commandInput

  case next of
    Just m -> do
      let mode' = lookupMode m
      draw mode'
      loop mode'
    Nothing -> return ()
