module Main where

main :: IO ()
main = do
  putStrLn "hello world"

data Trie a = TrieNode a [Trie a] deriving (Show, Eq)

type Comp   a b = a -> b -> Bool
type Update a b = a -> b -> b
type New    a b = a -> b

comp0 :: Char -> CharWeight -> Bool
comp0 c (CharWeight c' _) = c == c'

update0 :: Char -> CharWeight -> CharWeight
update0 c (CharWeight _ w) = CharWeight c (w + 1)

new0 :: Char -> CharWeight
new0 c = (CharWeight c 1)

insertCW = insertTrie comp0 update0 new0
insertCW2 = insertTrie comp0 update0 new0

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
    
data CharWeight = CharWeight Char Int deriving (Show, Eq)

buildBasicTrie :: [String] -> Trie CharWeight
buildBasicTrie = undefined
