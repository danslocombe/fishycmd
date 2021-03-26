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
import Complete.Git

import Prelude hiding (lookup)
import Data.List.Split
import System.Console.ANSI (Color(Red))
import Data.Maybe

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


instance Completer GitCompletionHandler where
  type CompleteType GitCompletionHandler = Char
  complete git p = if isGitCommand p
      then case shouldCompleteBranch ps of
        Just (p0, p1) -> let CompleterResult xs _ = complete (getBranchTries git) p1 in
          CompleterResult (prepend p0 <$> xs) Red
        Nothing -> CompleterResult [] Red
      else CompleterResult [] Red
      where
        ps = splitOn " " p
        prepend :: String -> Completion Char -> Completion Char
        prepend p0 (Completion x _) = Completion (p0 ++ x) 1

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
splitOnAdd splitS s = case splitOn splitS s of
  []  -> []
  [x] -> [x]
  xs  -> take (n-1) (map (++splitS) xs) ++ [last xs]
    where n = length xs

-- Split a string on another string and add the split string to the beginning
splitOnAddStart :: String -> String -> [String]
splitOnAddStart splitS s = case splitOn splitS s of
  []  -> []
  [x] -> [x]
  (x:xs)  -> x : map (splitS++) xs

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
            (filter (\(Completion x _) -> length x >= length p) rs) c) name



-- Ranking

-- Short term ranking
-- Long term ranking
