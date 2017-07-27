module Complete where

import FileTries
import Trie
import TrieState

import Data.Function (on)
import Data.List.Zipper
import Data.List.Split
import Data.List (maximumBy)
import Data.Maybe
import qualified Data.Map.Lazy as Map

fromTries :: [Trie CharWeight] -> String -> String
fromTries ts s = fmap fromCharWeight $ lookupCW s ts

-- Complete a prefix
complete :: FishyState -> String -> String
complete state currentDir = case length splitS of
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
      defOr x $ s ++ drop (length x) (fromTries (getFileTries state) x)
  where 
    splitS :: [String]
    splitS = splitOn " " s
    s :: String
    s = toList $ getPrompt state
    defOr s f = if s == "" then "" else f
    completeAll = rankTries (bigTrie state currentDir) s

-- Used for tab
partialComplete :: FishyState -> String -> String
partialComplete state currentDir = p ++ fromMaybe "" (listToMaybe split)
  where 
    p = toList $ getPrompt state
    c = complete state currentDir
    c' = drop (length p) c
    split = concatMap (splitOnAdd "\\") $ splitOnAdd "/" c'

splitOnAdd :: String -> String -> [String]
splitOnAdd split s = case splitOn split s of
  []  -> []
  [x] -> [x]
  xs  -> take (n-1) (map (++split) xs) ++ [last xs]
    where n = length xs

rankTries :: [[Trie CharWeight]] -> String -> String
rankTries ts p = fromMaybe p $ listToMaybe candidates
  where candidates = filter (\x -> length x > length p)$ map ((flip fromTries) p) ts

bigTrie :: FishyState -> String -> [[Trie CharWeight]]
bigTrie state currentDir = [getFileTries state 
  , Map.findWithDefault [] currentDir (getLocalizedHistoryTries state)
  , getHistoryTries state
  , getPathTries state]
