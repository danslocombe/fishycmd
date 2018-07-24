{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}

module Complete 
  ( allCompletions
  , splitCompletion
  , StringCompletion
  , StringCompleter      (..)
  , StringCompleterResult
  , Completion           (..)
  , CompleterName        (..)
  , FishyCompleterResult (..)
  , Completer
  ) where

import Complete.FileCompleter
import Complete.String
import Complete.Types
import Shell.State
import Shell.Types

import Prelude hiding (lookup)
import Data.Function (on)
import Data.List.Zipper
import Data.List.Split
import Data.List (maximumBy)
import System.Console.ANSI (Color(Red))
import Data.Maybe
import qualified Data.Map.Lazy as Map

instance Completer [StringTrie] where
  type CompleteType [StringTrie] = Char
  complete ts p = CompleterResult ss Red
    where
        ss :: [StringCompletion]
        ss = (Completion . fmap fromCharWeight) <$> allMatches p ts 

splitCompletion :: String -> String -> String
splitCompletion p c = p ++ compl
                where
                  c' = drop (length p) c
                  compl = fromMaybe "" $ listToMaybe $ split'
                  split = concatMap (splitOnAddStart " ") .
                          concatMap (splitOnAdd "\\") .
                          splitOnAdd "/"
                  split' = case split c' of
                    ("":xs) -> xs
                    (" ":xs) -> xs
                    ys -> ys

-- Split a string on another string and add the split string to the end
splitOnAdd :: String -> String -> [String]
splitOnAdd split s = case splitOn split s of
  []  -> []
  [x] -> [x]
  xs  -> take (n-1) (map (++split) xs) ++ [last xs]
    where n = length xs

-- Split a string on another string and add the split string to the beginning
splitOnAddStart :: String -> String -> [String]
splitOnAddStart split s = case splitOn split s of
  []  -> []
  [x] -> [x]
  (x:xs)  -> x : map (split++) xs

-- Given a list of completers and a prefix produce a list of results
-- TODO : Use lenses
allCompletions :: [StringCompleter] -> String -> [FishyCompleterResult]
allCompletions cs p = map (filterResults . applyComplete) cs
  where
    applyComplete (StringCompleter x name) = FishyCompleterResult (complete x p) name
    filterResults (FishyCompleterResult (CompleterResult rs c) name) 
      = FishyCompleterResult 
          (CompleterResult 
            (filter (\(Completion c) -> length c >= length p) rs) c) name
