{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Completer where

import System.Console.ANSI.Types

class Completer c where
  type CompleteType c :: *
  complete :: c -> [CompleteType c] -> ([CompleteType c], Color)
