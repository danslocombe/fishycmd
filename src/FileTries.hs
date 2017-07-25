{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}

module FileTries where

import Trie

import GHC.Generics
import Data.Serialize
import System.Directory
import System.Environment
    
data CharWeight = CharWeight Char Int deriving (Generic, Show, Eq)

instance Serialize CharWeight

instance Ord CharWeight where
  (CharWeight _ p) `compare` (CharWeight _ p') = p `compare` p'

stripQuotes :: String -> String
stripQuotes = filter (/= '"')

stripDoubleBackslash :: String -> String
stripDoubleBackslash [] = []
stripDoubleBackslash ('\\':'\\':xs) = '\\' : stripDoubleBackslash xs
stripDoubleBackslash (x:xs) = x : stripDoubleBackslash xs

parseFilename :: String -> String
parseFilename = stripQuotes . stripDoubleBackslash

-- Build a standard trie for strings
buildTries :: [String] -> [Trie CharWeight]
buildTries files = foldr insertCW [] (fmap (parseFilename . show) files)

comp0 :: Char -> CharWeight -> Bool
comp0 c (CharWeight c' _) = c == c'

update0 :: Char -> CharWeight -> CharWeight
update0 c (CharWeight _ w) = CharWeight c (w + 1)

new0 :: Char -> CharWeight
new0 c = (CharWeight c 1)

insertCW = insertTrie comp0 update0 new0

lookupCW = lookupTrie comp0

fromCharWeight (CharWeight c _) = c
