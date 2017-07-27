{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}

module Complete where

import Completer
import FileCompleter
import StringTries
import Trie
import TrieState

import Data.Function (on)
import Data.List.Zipper
import Data.List.Split
import Data.List (maximumBy)
import Data.Maybe
import qualified Data.Map.Lazy as Map

instance Completer [Trie CharWeight] where
  type CompleteType [Trie CharWeight] = Char
  complete = fromTries

data StringCompleter = forall c. (Completer c, CompleteType c ~ Char) => StringCompleter c

fromTries :: [Trie CharWeight] -> String -> String
fromTries ts s = fmap fromCharWeight $ lookupCW s ts

-- Complete a prefix
fishyComplete :: FishyState -> String -> String
fishyComplete state currentDir = case length splitS of
  -- Don't do anything for empty string
  0 -> ""

  -- For a single 'word' use all tries
  1 -> defOr s $ completeAll

  -- For multiple 'words'
  _ -> if length completeAll > length s
    -- Prefer history, otherwise complete on filenames
    then completeAll
    -- Otherwise use files in current directory
    else let x = last splitS in 
      defOr x $ s ++ drop (length x) (complete (getFileCompleter state) x)
  where 
    splitS :: [String]
    splitS = splitOn " " s
    s :: String
    s = toList $ getPrompt state
    defOr s f = if s == "" then "" else f
    completeAll = rankTries (allCompleters state currentDir) s

-- Used for tab
fishyPartialComplete :: FishyState -> String -> String
fishyPartialComplete state currentDir = p ++ fromMaybe "" (listToMaybe split)
  where 
    p = toList $ getPrompt state
    c = fishyComplete state currentDir
    c' = drop (length p) c
    split = concatMap (splitOnAddStart " ") $ concatMap (splitOnAdd "\\") $ splitOnAdd "/" c'

splitOnAdd :: String -> String -> [String]
splitOnAdd split s = case splitOn split s of
  []  -> []
  [x] -> [x]
  xs  -> take (n-1) (map (++split) xs) ++ [last xs]
    where n = length xs

splitOnAddStart :: String -> String -> [String]
splitOnAddStart split s = case splitOn split s of
  []  -> []
  [x] -> [x]
  (x:xs)  -> x : map (split++) xs

rankTries :: [StringCompleter] -> String -> String
rankTries cs p = fromMaybe p $ listToMaybe candidates
  where candidates = filter (\x -> length x > length p) 
                   $ map (\(StringCompleter x) -> complete x p) cs

allCompleters :: FishyState -> String -> [StringCompleter]
allCompleters state currentDir = 
  [ StringCompleter $ Map.findWithDefault [] currentDir (getLocalizedHistoryTries state)
  , StringCompleter $ getFileCompleter state 
  , StringCompleter $ getHistoryTries state
  , StringCompleter $ getPathTries state]
