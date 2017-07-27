{-# LANGUAGE MultiParamTypeClasses #-}

module Completer where

class Completer a c where
  complete :: c -> [a] -> [a]
