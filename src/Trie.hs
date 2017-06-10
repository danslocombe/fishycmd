module Trie where

data Trie a = TrieNode a [Trie a] deriving (Show, Eq, Ord)

type Comp   a b = a -> b -> Bool
type Update a b = a -> b -> b
type New    a b = a -> b


data CharWeight = CharWeight Char Int deriving (Show, Eq)
instance Ord CharWeight where
  (CharWeight _ p) `compare` (CharWeight _ p') = p `compare` p'

comp0 :: Char -> CharWeight -> Bool
comp0 c (CharWeight c' _) = c == c'

update0 :: Char -> CharWeight -> CharWeight
update0 c (CharWeight _ w) = CharWeight c (w + 1)

new0 :: Char -> CharWeight
new0 c = (CharWeight c 1)

insertCW = insertTrie comp0 update0 new0

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
    

buildBasicTrie :: [String] -> Trie CharWeight
buildBasicTrie = undefined

lookupCW = lookupTrie comp0

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

fromCharWeight (CharWeight c _) = c
-- genString = fmap fromCharWeight

bestEntry :: Ord a => [Trie a] -> [a]
bestEntry [] = []
bestEntry ts = best:(bestEntry children)
  where
    (TrieNode best children) = maximum ts 
