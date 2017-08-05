{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Complete.Completer where

import System.Console.ANSI.Types

newtype Completion a = Completion [a] deriving Show
data CompleterResult a = CompleterResult [Completion a] Color deriving Show

-- data Completion where
  -- Completion :: Completer c => [CompleteType c] -> Completion c
-- 
-- data CompleterResult c where
  -- CompleterResult :: Completer c => [Completion c] -> Color -> CompleterResult c
-- 
-- data CompleterResult c = CompleterResult [Completej c] Color

class Completer c where
  type CompleteType c :: *
  complete :: c -> [CompleteType c] -> CompleterResult (CompleteType c)
