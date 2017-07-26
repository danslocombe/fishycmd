module Complete where

import FileTries
import Trie
import TrieState

import Data.List.Zipper
import Data.Maybe
import Data.List.Split

fromTries :: [Trie CharWeight] -> String -> String
fromTries ts s = fmap fromCharWeight $ lookupCW s ts

-- Complete a prefix
complete :: FishyState -> String
complete state = case length splitS of
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
    completeAll = fromTries (bigTrie state) s

-- Used for tab
partialComplete :: FishyState -> String
partialComplete state = p ++ fromMaybe "" (listToMaybe split)
  where 
    p = toList $ getPrompt state
    c = complete state
    c' = drop (length p) c
    split = concatMap (splitOnAdd "\\") $ splitOnAdd "/" c'

splitOnAdd :: String -> String -> [String]
splitOnAdd split s = case splitOn split s of
  []  -> []
  [x] -> [x]
  xs  -> take (n-1) (map (++split) xs) ++ [last xs]
    where n = length xs

bigTrie :: FishyState -> [Trie CharWeight]
bigTrie state = getFileTries state ++ getHistoryTries state ++ getPathTries state

