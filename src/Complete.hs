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
import Complete.Trie
import Complete.Types
import CLI.State
import CLI.Types

import Prelude hiding (lookup)
import Data.Function (on)
import Data.List.Zipper
import Data.List.Split
import Data.List (maximumBy)
import System.Console.ANSI (Color(Red))
import Data.Maybe
import qualified Data.Map.Lazy as Map

toCompletion :: [CharWeight] -> Completion Char
toCompletion cw = Completion (cwToString cw) (lastOrZero $ getWeight <$> cw)
  where
    lastOrZero :: [Int] -> Int
    lastOrZero [] = 0
    lastOrZero xs = last xs

instance Completer [StringTrie] where
  type CompleteType [StringTrie] = Char
  complete ts p = CompleterResult (toCompletion <$> matches) Red
    where
        matches = allTrieMatches p ts

instance Completer FileCompleter where
  type CompleteType FileCompleter = Char
  complete (FileCompleter _ fs) prefix = CompleterResult cs Red
    where 
      -- cs :: [String]
      cs = (\x -> Completion x 1) <$> filter (Complete.FileCompleter.startsWith prefix) fs


-- Split completion for partial completion
splitCompletion :: String -> String -> String
splitCompletion p c = p ++ compl
                -- given a prefix and a completion, we return the prefix with the split completion
                -- TODO also pass in context to determine when we need to split
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
    applyComplete (StringCompleter x name) = 
      FishyCompleterResult (complete x p) name

    filterResults (FishyCompleterResult (CompleterResult rs c) name) 
      = FishyCompleterResult 
          (CompleterResult 
            (filter (\(Completion c _) -> length c >= length p) rs) c) name



-- Ranking

-- Short term ranking
-- Long term ranking
