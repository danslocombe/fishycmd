{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Completer where

class Completer c where
  type CompleteType c :: *
  complete :: c -> [CompleteType c] -> [CompleteType c]
