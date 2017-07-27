{-# LANGUAGE MultiParamTypeClasses #-}

module Completer where

class Completer a c where
  complete :: c -> [a] -> [a]

-- class Completer c where
  -- type CompleteType c :: *
  -- complete :: c -> [CompleteType c] -> [CompleteType c]
