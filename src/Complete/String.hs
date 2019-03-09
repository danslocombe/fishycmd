{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Complete.String
  ( StringTrie
  , buildTries
  , parseFilename
  , CharWeight (..)
  , cwToChar
  , cwToString
  ) where

import Complete.Trie

import Prelude hiding (lookup)
import GHC.Generics
import Data.Serialize
import Safe
import System.Directory
import System.Environment
    
type StringTrie = Trie CharWeight
data CharWeight = CharWeight 
  { getChar :: Char
  , getWeight :: Int
  } deriving (Generic, Show, Eq)

instance Serialize CharWeight

instance Ord CharWeight where
  (CharWeight _ p) `compare` (CharWeight _ p') = p `compare` p'

instance ConcreteTrie Char CharWeight where
  comp c (CharWeight c' _) = c == c'
  update c (CharWeight _ w) = CharWeight c (w + 1)
  new c = (CharWeight c 1)
  finalHeuristic t = 
    3 * (getFinal t) > (maxChildWeight)
    where
      maxChildWeight = maxorzero $ (getWeight . getData) <$> getChildren t

      maxorzero [] = 0
      maxorzero xs = maximum xs

-- --- --- -- -- -  - - -- - --- -- --- --- -- 

stripQuotes :: String -> String
stripQuotes = filter (/= '"')

stripDoubleBackslash :: String -> String
stripDoubleBackslash [] = []
stripDoubleBackslash ('\\':'\\':xs) = '\\' : stripDoubleBackslash xs
stripDoubleBackslash (x:xs) = x : stripDoubleBackslash xs

parseFilename :: String -> String
parseFilename = stripQuotes . stripDoubleBackslash

escapeSpace :: String -> String
escapeSpace = concatMap (\x -> case x of 
    ' ' -> "\\ "
    y -> [y])

-- --- --- -- -- -  - - -- - --- -- --- --- -- 

-- Build a standard trie for strings
buildTries :: [String] -> [Trie CharWeight]
buildTries files = foldr insertTrie [] $ fmap parseFilename files

cwToChar :: CharWeight -> Char
cwToChar (CharWeight c _) = c

cwToString :: [CharWeight] -> String
cwToString = fmap cwToChar

buildFileTries :: String -> IO [Trie CharWeight]
buildFileTries dir = buildTries <$> (listDirectory =<< getCurrentDirectory)
