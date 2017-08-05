{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}

module Complete 
  ( fishyComplete
  , toDraw
  , StringCompletion
  , Completion           (..)
  , CompleterName        (..)
  , FishyCompleterResult (..)
  ) where

import Complete.Completer
import Complete.FileCompleter
import Complete.String
import Shell.State

import Prelude hiding (lookup)
import Data.Function (on)
import Data.List.Zipper
import Data.List.Split
import Data.List (maximumBy)
import System.Console.ANSI (Color(Red))
import Data.Maybe
import qualified Data.Map.Lazy as Map

data StringCompleter = 
  forall c. (Completer c, CompleteType c ~ Char) =>
    StringCompleter c CompleterName

type StringCompletion = Completion Char

type StringCompleterResult = CompleterResult Char

data FishyCompleterResult = FishyCompleterResult StringCompleterResult CompleterName

data CompleterName = NameLocalHistoryCompleter
                   | NameFileCompleter
                   | NameGlobalHistoryCompleter
                   | NamePathCompleter

instance Completer [StringTrie] where
  type CompleteType [StringTrie] = Char
  -- complete ts x = (fromTries ts x, Red)
  complete ts p = CompleterResult ss Red
    where
        ss :: [StringCompletion]
        ss = (Completion . fmap fromCharWeight) <$> allMatches p ts 

fromTries :: [StringTrie] -> String -> String
fromTries ts s = fmap fromCharWeight $ lookup s ts

--toDraw :: FishyState -> String -> [(String, Color)]
--toDraw state currentDir = case length splitS of
toDraw :: [FishyCompleterResult] -> String -> (StringCompletion, Color)
toDraw cs s = case length splitS of
  -- Don't do anything for empty string
  -- 0 -> (Completion "", Red)

  -- For a single 'word' use all tries
  _ -> defOr s $ case  (\(FishyCompleterResult x _) -> x) <$> firstFCR cs of
    Just res -> firstResult res
    Nothing -> (Completion "", Red)
-- 
  -- -- For multiple 'words'
  -- _ -> [if length completeAll > length s
    -- -- Prefer history, otherwise complete on filenames
    -- then completeAll
    -- -- Otherwise use files in current directory
    -- else let x = last splitS in 
         -- let (CompleterResult compls color) = complete (getFileCompleter state) x in
         -- let (Completion compl) = head compls in
         -- defOr x $ (s ++ drop (length x) compl, color)]
  -- where 
  where
    firstFCR :: [FishyCompleterResult] -> Maybe FishyCompleterResult
    firstFCR fcrs = listToMaybe $ filter 
      (\(FishyCompleterResult (CompleterResult ccs _) _) -> length ccs > 0) fcrs
    firstResult :: StringCompleterResult -> (StringCompletion, Color)
    firstResult (CompleterResult cs color) = (fromMaybe (Completion "") $ listToMaybe cs, color)
    splitS :: [String]
    splitS = splitOn " " s
    defOr s f = if s == "" then (Completion "", Red) else f
    -- -- completeAll = rankTries (allCompleters state currentDir) s
    -- completeAll = undefined

-- Complete a prefix
fishyComplete :: FishyState -> String -> [FishyCompleterResult]
fishyComplete state currentDir = allCompletions (allCompleters state currentDir) s
  where
    s :: String
    s = toList $ getPrompt state

splitCompletion :: String -> [String]
splitCompletion = concatMap (splitOnAddStart " ") .
                  concatMap (splitOnAdd "\\") .
                  splitOnAdd "/"

-- Used for tab
-- fishyPartialComplete :: FishyState -> String -> String
-- fishyPartialComplete state currentDir = p ++ fromMaybe "" (listToMaybe split)
  -- where 
    -- p = toList $ getPrompt state
    -- (c, _) = fishyComplete state currentDir
    -- c' = drop (length p) c
    -- split = concatMap (splitOnAddStart " ") $ concatMap (splitOnAdd "\\") $ splitOnAdd "/" c'

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

-- rankTries :: [StringCompleter] -> String -> (String, Color)
-- rankTries cs p = fromMaybe (p, Red) $ listToMaybe candidates
  -- where candidates = filter (\(x,_) -> length x > length p) 
                   -- $ map (\(StringCompleter x) -> complete x p) cs
                   --
-- TODO : Use lenses
allCompletions :: [StringCompleter] -> String -> [FishyCompleterResult]
allCompletions cs p = map (filterResults . applyComplete) cs
  where
    applyComplete (StringCompleter x name) = FishyCompleterResult (complete x p) name
    filterResults (FishyCompleterResult (CompleterResult rs c) name) 
      = FishyCompleterResult 
          (CompleterResult 
            (filter (\(Completion c) -> length c > length p) rs) c) name

allCompleters :: FishyState -> String -> [StringCompleter]
allCompleters state currentDir = 
  [ StringCompleter local                    NameLocalHistoryCompleter
  , StringCompleter (getFileCompleter state) NameFileCompleter
  , StringCompleter (getHistoryTries  state) NameGlobalHistoryCompleter
  , StringCompleter (getPathTries     state) NamePathCompleter]
  where local = Map.findWithDefault [] currentDir $ getLocalizedHistoryTries state
