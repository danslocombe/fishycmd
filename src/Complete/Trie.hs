{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Complete.Trie where

import GHC.Generics
import Data.Serialize
import Data.List (sort)

data Trie a = TrieNode a [Trie a] deriving (Generic, Show, Eq, Ord)

instance (Serialize a) => Serialize (Trie a)

type Comp   a b = a -> b -> Bool
type Update a b = a -> b -> b
type New    a b = a -> b

class ConcreteTrie a b where
  comp   :: Comp a b
  update :: Update a b
  new    :: New a b


insertTrie :: (ConcreteTrie a b, Eq b) => [a] -> [Trie b] -> [Trie b]
insertTrie [] ts = ts
insertTrie (x:xs) ts = ret 
  where ts' = fmap upup ts 
        comptrie = \(TrieNode y _) -> comp x y
        upup = \t@(TrieNode v children) -> 
          if comptrie t 
            then let children' = insertTrie xs children
              in TrieNode (update x v) children'
            else t
        -- This is bad, replace with monad?
        ret = if ts' == ts then ts ++ [TrieNode (new x) (insertTrie xs [])] else ts'

allLists :: Ord b => [Trie b] -> [[b]]
allLists ts = concatMap f ts
  where 
    f :: Ord b => Trie b -> [[b]]
    f (TrieNode x []) = [[x]]
    f (TrieNode x cs) = fmap (\ls -> x : ls) (allLists cs)

allTrieMatches :: (ConcreteTrie a b, Ord b) => [a] -> [Trie b] -> [[b]]
allTrieMatches [] ts = allLists ts
allTrieMatches (x:xs) ts = ret
  where
    -- TODO factor comptrie out
    comptrie = \(TrieNode y _) -> comp x y
    tings = filter comptrie ts
    ret = case tings of 
      [] -> []
      ((TrieNode y children):ys) -> fmap (\x -> y:x) (allTrieMatches xs children)

lookupTrie :: (ConcreteTrie a b, Ord b) => [a] -> [Trie b] -> [b]
lookupTrie [] ts = bestEntry ts
lookupTrie (x:xs) ts = ret
  where
    -- TODO factor comptrie out
    comptrie = \(TrieNode y _) -> comp x y
    tings = filter comptrie ts
    ret = case tings of 
      [] -> []
      ((TrieNode y children):ys) -> y:(lookupTrie xs children)

bestEntry :: Ord a => [Trie a] -> [a]
bestEntry [] = []
bestEntry ts = best:(bestEntry children)
  where
    (TrieNode best children) = maximum ts 
