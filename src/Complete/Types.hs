{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}

module Complete.Types where

import System.Console.ANSI.Types

data Completion a = Completion [a] Int deriving Show

instance Eq a => Eq (Completion a) where
  (Completion x _n) == (Completion y _m) = x == y

data CompleterResult a = CompleterResult [Completion a] Color deriving Show

class Completer c where
  type CompleteType c :: *
  complete :: c -> [CompleteType c] -> CompleterResult (CompleteType c)

data StringCompleter = 
  forall c. (Completer c, CompleteType c ~ Char) =>
    StringCompleter c CompleterName

type StringCompletion = Completion Char

type StringCompleterResult = CompleterResult Char

data FishyCompleterResult = FishyCompleterResult StringCompleterResult CompleterName 
  deriving Show

data CompleterName = NameLocalHistoryCompleter
                   | NameGitCompleter
                   | NameFileCompleter
                   | NameGlobalHistoryCompleter
                   | NamePathCompleter deriving (Eq, Show)