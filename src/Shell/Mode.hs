{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Mode where

import Control.Monad.RWS
import Control.Monad.RWS.Class

class Mode where
  internalAction :: MonadState FishyState m => m ()
  draw :: IO ()

