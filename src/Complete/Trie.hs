{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}

module Complete.Trie where

import GHC.Generics
import Data.Serialize
import Data.List (sort)

data Trie a = TrieNode a [Trie a] deriving (Generic, Show, Eq, Ord)

instance (Serialize a) => Serialize (Trie a)

type Comp   a b = a -> b -> Bool
type Update a b = a -> b -> b
type New    a b = a -> b

insertTrie :: Eq b => Comp a b -> Update a b -> New a b -> [a] -> [Trie b] -> [Trie b]
insertTrie _ _ _ [] ts = ts
insertTrie comp update new (x:xs) ts = ret 
  where ts' = fmap upup ts 
        partialInsert = insertTrie comp update new
        comptrie = \(TrieNode y _) -> comp x y
        upup = \t@(TrieNode v children) -> 
          if comptrie t 
            then let children' = partialInsert xs children
              in TrieNode (update x v) children'
            else t
        -- This is bad, replace with monad?
        ret = if ts' == ts then ts ++ [TrieNode (new x) (partialInsert xs [])] else ts'

allLists :: Ord b => [Trie b] -> [[b]]
allLists ts = concatMap f ts
  where 
    f :: Ord b => Trie b -> [[b]]
    f (TrieNode x []) = [[x]]
    f (TrieNode x cs) = fmap (\ls -> x : ls) (allLists cs)

allTrieMatches :: Ord b => Comp a b -> [a] -> [Trie b] -> [[b]]
allTrieMatches _ [] ts = allLists ts
allTrieMatches comp (x:xs) ts = ret
  where
    -- TODO factor comptrie out
    comptrie = \(TrieNode y _) -> comp x y
    tings = filter comptrie ts
    ret = case tings of 
      [] -> []
      ((TrieNode y children):ys) -> fmap (\x -> y:x) (allTrieMatches comp xs children)

lookupTrie :: Ord b => Comp a b -> [a] -> [Trie b] -> [b]
lookupTrie _ [] ts = bestEntry ts
lookupTrie comp (x:xs) ts = ret
  where
    -- TODO factor comptrie out
    comptrie = \(TrieNode y _) -> comp x y
    tings = filter comptrie ts
    ret = case tings of 
      [] -> []
      ((TrieNode y children):ys) -> y:(lookupTrie comp xs children)

bestEntry :: Ord a => [Trie a] -> [a]
bestEntry [] = []
bestEntry ts = best:(bestEntry children)
  where
    (TrieNode best children) = maximum ts 
