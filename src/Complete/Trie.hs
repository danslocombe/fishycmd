{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}

module Complete.Trie where

import GHC.Generics
import Data.Serialize
import Data.List (sort)

data Trie a = TrieNode 
  { getData :: a
  , getChildren :: [Trie a]

  -- The number of inserted xs : [a] that end exactly on this node
  , getFinal :: Int

  } deriving (Generic, Show, Eq, Ord)

-- Trie typeclass, deals with data of type 'a' stored in the
-- tries as type 'b'
class Trieable a b | b -> a where
  -- Compare from some 'a' outside the tries and a 'b' already in a trie
  comp   :: a -> b -> Bool

  -- Update the 'b' with information from 'a'
  update :: a -> b -> b

  new    :: a -> b

  -- If we have inserted abcd and abcde into a trie and
  -- we have input abc, when should we return the former and when the latter?
  -- finalHeuristic determines when we return the truncated string
  finalHeuristic :: Trie b -> Bool

instance (Serialize a) => Serialize (Trie a)

-- --- --- -- -- -  - - -- - --- -- --- --- -- 

compTrie :: (Trieable a b, Eq b) => a -> Trie b -> Bool
compTrie x t = comp x $ getData t

insertTrie :: (Trieable a b, Eq b) => [a] -> [Trie b] -> [Trie b]
insertTrie [] ts = ts
insertTrie (x:xs) ts = ret 
  where -- We run upup over the child nodes
        -- If there is a match then we recurse down that child
        ts' = fmap upup ts 
        upup t@(TrieNode v children f) =
          if compTrie x t 
            then 
              if null xs
                then t { getData = update x v
                       -- Only update the node's 'final' when this is the endpoint for insertion
                       -- i.e. the end of the string
                       , getFinal = f + 1
                       }
                else t { getData = update x v
                       , getChildren = insertTrie xs children
                       }
            else t

        -- If we have not updated any of the children then we need to insert
        -- an additional node
        ret = if ts' == ts 
          then ts ++ [
            TrieNode 
            { getData = new x
            , getChildren = insertTrie xs [] 
            , getFinal = if null xs then 1 else 0
            }]
          else ts'

allLists :: forall a b. (Trieable a b) => [Trie b] -> [[b]]
allLists ts = concatMap f ts
  where 
    f :: Trie b -> [[b]]
    f (TrieNode x [] f) = [[x]]
    f t@(TrieNode x cs f) = (\ls -> x : ls) <$> addCurrent t ++ (allLists cs)
    addCurrent t = if finalHeuristic t then [[]] else []

allTrieMatches :: (Trieable a b, Eq b) => [a] -> [Trie b] -> [[b]]
allTrieMatches [] ts = allLists ts
allTrieMatches (x:xs) ts = ret
  where
    matching = filter (compTrie x) ts
    ret = case matching of 
      [] -> []
      (t@(TrieNode y children f):ys) ->
         ((\x -> y:x) <$> addCurrent t ++ (allTrieMatches xs children))

    addCurrent t = if finalHeuristic t then [[]] else []

lookupTrie :: (Trieable a b, Ord b) => [a] -> [Trie b] -> [b]
lookupTrie [] ts = bestEntry ts
lookupTrie (x:xs) ts = ret
  where
    matching = filter (compTrie x) ts
    ret = case matching of 
      [] -> []
      (t@(TrieNode y children f):ys) -> y:(rest t)
    rest t = if finalHeuristic t 
      then []
      else lookupTrie xs (getChildren t)

bestEntry :: (Trieable a b, Ord b) => [Trie b] -> [b]
bestEntry [] = []
bestEntry ts = best:rest
  where
    t@(TrieNode best children f) = maximum ts 
    rest = if finalHeuristic t then [] else bestEntry children
