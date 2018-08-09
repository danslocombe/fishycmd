{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module Search where

import GHC.Generics
import Data.Serialize
import Data.Maybe (isNothing, fromJust, maybeToList, catMaybes)
import qualified Data.Map.Strict as Map
import Data.List.Split (splitOn)
import Data.List (intersperse, sortBy, inits)

type Dist = Int

class Metric a where
  -- We assume d follows the metric space axioms
  d :: a -> a -> Dist

-- Define edit distance
-- https://en.wikipedia.org/wiki/Levenshtein_distance
-- We use this which includes transposing chars
-- https://en.wikipedia.org/wiki/Damerau%E2%80%93Levenshtein_distance
instance Metric [Char] where
  d s s' = levenshtein s s' (length s) (length s')
    
levenshtein :: Eq a => [a] -> [a] -> Int -> Int -> Dist
levenshtein _ _ 0 x = x
levenshtein _ _ x 0 = x

levenshtein s s' i j = minimum $ [d0,d1,d2] ++ maybeToList d3
  where
    d0 = (levenshtein s s' (i-1) j) + 1
    d1 = (levenshtein s s' i (j-1)) + 1
    d2 = (levenshtein s s' (i-1) (j-1)) 
       + (ind (s !! (i-1)) (s' !! (j-1)))

    ind x y = if x == y then 0 else 1

    -- swaps
    d3 = if i > 1 && j > 1 && 
           (s !! (i-1)) == (s' !! (j-2)) &&
           (s !! (i-2)) == (s' !! (j-1))
      then Just $ levenshtein s s' (i-2) (j-2) + 1
      else Nothing


-- No point in being generic, could in theory have Eq a => [a] instead of strings
-- Here a represents the payload
data BKTree a = BKTree
  { bkString :: String
  , bkPayload :: a
  , bkMap :: ! (Map.Map Int (BKTree a))
  } deriving Generic

instance Serialize a => Serialize (BKTree a)

instance Show a => Show (BKTree a) where
  show BKTree{..} = "(" 
                 ++ bkString
                 ++ ", "
                 ++ (show bkPayload)
                 ++ ", "
                 ++ (show bkMap)
                 ++ ")"
    where 
      f = unlines . (map ("  "++)) . lines
      ks = Map.keys bkMap 
      children = Map.keys bkMap -- fromJust . (flip Map.lookup bkMap) <$> ks

bkTest :: [String] -> BKTree ()
bkTest [] = undefined
bkTest (s:ss) = foldl (\bk s -> bkInsert s () bk) (bkNew s ()) ss

bkNew :: String -> a -> BKTree a
bkNew s x = BKTree
  { bkString = s
  , bkPayload = x
  , bkMap = Map.empty
  }

bkInsert :: String -> a -> BKTree a -> BKTree a
bkInsert s x t =
  case Map.lookup dist map of
    Just t' -> t {bkMap = Map.adjust (bkInsert s x) dist map}
    Nothing -> t {bkMap = Map.insert dist new map}
  where
    dist = d s (bkString t)
    new = bkNew s x
    map = bkMap t

bkLookup :: Dist -> String -> BKTree a -> [(String, a, Dist)]
bkLookup dist s t = direct ++ concatMap (bkLookup curDist s) children
  where
    s' = bkString t
    x = bkPayload t
    map = bkMap t
    curDist = d s s'
    direct = if curDist < dist then [(s', x, curDist)] else []
    maxDist = curDist + dist
    minDist = curDist - dist
    keys = filter (\k -> k > minDist && k < maxDist) (Map.keys map)
    children = (\k -> fromJust $ Map.lookup k map) <$> keys

-- todo
-- bkOptimize rotate tree by some heuristic 

bkUpdate :: String -> (a -> a) -> BKTree a -> Maybe (BKTree a)
bkUpdate s f t = if dist == 0
  then Just t {bkPayload = f (bkPayload t)}
  else do
    t' <- Map.lookup dist map 
    t'' <- bkUpdate s f t'
    return $ t {bkMap = Map.insert dist t'' map}
  where
    dist = d s (bkString t)
    map = bkMap t

bkUpdateDefault :: String -> (a -> a) -> a -> BKTree a -> BKTree a
bkUpdateDefault s f def t = case bkUpdate s f t of
  Just t' -> t'
  Nothing -> bkInsert s def t

type ArchiveId = Int
type ArchiveScore = Int

data ArchivedCommand = ArchivedCommand String ArchiveScore deriving Show

data HistoryIndex = HistoryIndex 
  { hiTrie :: BKTree [ArchiveId]
  , hiArchive :: Map.Map ArchiveId ArchivedCommand
  , hiNextId :: ArchiveId
  , hiLookupMaxDist :: Dist
  } deriving Show

hiNew :: HistoryIndex
hiNew = HistoryIndex
  { hiTrie = bkNew "fishy" [0]
  , hiArchive = Map.fromList [(0, ArchivedCommand "echo \"hello from fishy search!\"" 1)]
  , hiNextId = 1
  , hiLookupMaxDist = 4
  }

hiLookup :: HistoryIndex -> String -> [(String, ArchivedCommand, Dist)]
hiLookup HistoryIndex{..} s = x
  where
    l0 = bkLookup hiLookupMaxDist s hiTrie :: [(String, [ArchiveId], Dist)]
    ids = concatMap (\(s, x, p) -> zip3 (repeat s) x (repeat p)) l0
    x = catMaybes $ process <$> ids

    process (match,id,penalty) = do
      archived <- Map.lookup id hiArchive
      return (match, archived, penalty)

penaltyMultiplier = 2

hiLookupCommand :: HistoryIndex -> String -> [(String, [String], Int)]
hiLookupCommand h c = sortBy (\(_, _, x) (_, _, y) -> y `compare` x) list'
  where
    split = splitCommandForIndexing . trimCommand
    ss = split c
    results = concatMap (hiLookup h) ss
    finalScoreMap = foldl f Map.empty results

    -- I'm so sorry
    f :: Map.Map String (Int, Int, [String]) -> (String, ArchivedCommand, Dist) -> Map.Map String (Int, Int, [String])
    f scoreMap (match, ArchivedCommand command score, penalty) =
      let def = (score, penalty, [match]) in
      Map.insertWith (\(s, p, ms) (s', p', ms') -> (s + s', min p p', ms ++ ms')) command def scoreMap

    list :: [(String, [String], Int, Int)]
    list = (\(x, (score, penalty, z)) -> (x, z, score, penalty)) <$> Map.assocs finalScoreMap
    list' :: [(String, [String], Int)]
    list' = (\(x, y, score, penalty) -> (x, y, score - penaltyMultiplier * penalty)) <$> list

    

hiNewCommand :: HistoryIndex -> String -> HistoryIndex
hiNewCommand h s = ret
  where
    split = splitCommandForIndexing . trimCommand
    ss = split s
    archive = hiArchive h
    reconstructed = concat $ intersperse " " ss
    archiveKVs = Map.assocs archive

    increment :: ArchivedCommand -> ArchivedCommand
    increment (ArchivedCommand s x) = ArchivedCommand s (x + 1)

    ret = case filter (\(_, ArchivedCommand v _) -> v == reconstructed) archiveKVs of
      -- Found the command already in the archive
      -- update its count and then get out
      (k, _):_ -> h {hiArchive = Map.adjust increment k archive}
      -- Haven't found the command
      -- Add to archive
      -- Add to trie
      _ -> h {hiArchive = archive', hiTrie = trie', hiNextId = id+1}

    id = hiNextId h
    archive' = Map.insert id (ArchivedCommand reconstructed 1) archive
    trie = hiTrie h
    -- Iterate through split up command adding the id of the archive entry to locations
    -- in the trie
    ssWithPrefixes = filter (not . null) $ concat $ inits <$> ss
    trie' = foldl (\t s -> bkUpdateDefault s (id:) [id] t) trie ssWithPrefixes

hiFromCommands :: [String] -> HistoryIndex
hiFromCommands = foldl hiNewCommand hiNew



--- --- --- These are hacky --- --- -- ---- -
--- Really we need a module for splitting ---

trimCommand :: String -> String
trimCommand = f . f
  where f = reverse . dropWhile (== ' ')

-- TODO handle quotes correctly
splitCommandForIndexing :: String -> [String]
splitCommandForIndexing = filter (/= []) . splitOn " "

-- TODO extra splitting on camelcase, / \\ etc for INDEXING POWER!!
