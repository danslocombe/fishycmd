{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}

module Complete.String
  ( StringTrie
  , insert
  , lookup
  , allMatches
  , buildTries
  , fromCharWeight
  , parseFilename
  ) where

import Complete.Trie

import Prelude hiding (lookup)
import GHC.Generics
import Data.Serialize
import Safe
import System.Directory
import System.Environment
    
type StringTrie = Trie CharWeight
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
buildTries files = foldr insert [] $ fmap parseFilename files

fromCharWeight :: CharWeight -> Char
fromCharWeight (CharWeight c _) = c

buildFileTries :: String -> IO [Trie CharWeight]
buildFileTries dir = buildTries <$> (listDirectory =<< getCurrentDirectory)

escapeSpace :: String -> String
escapeSpace = concatMap (\x -> case x of 
    ' ' -> "\\ "
    y -> [y])

-- Trie functions for CharWeight

compCW :: Char -> CharWeight -> Bool
compCW c (CharWeight c' _) = c == c'

updateCW :: Char -> CharWeight -> CharWeight
updateCW c (CharWeight _ w) = CharWeight c (w + 1)

newCW :: Char -> CharWeight
newCW c = (CharWeight c 1)

insert :: String -> [Trie CharWeight] -> [Trie CharWeight]
insert = insertTrie compCW updateCW newCW

lookup :: String -> [Trie CharWeight] -> [CharWeight]
lookup = lookupTrie compCW

allMatches :: String -> [Trie CharWeight] -> [[CharWeight]]
allMatches = allTrieMatches compCW
