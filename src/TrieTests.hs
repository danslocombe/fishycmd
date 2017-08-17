{-# LANGUAGE FlexibleInstances #-}

module Test.Trie where

import Prelude hiding (lookup)
import Test.QuickCheck
import Control.Monad
import Data.List (nub)

import Complete.Trie
import Complete.String

prop_RevRev :: Eq a => [a] -> Bool
prop_RevRev xs = reverse (reverse xs) == xs

instance Arbitrary CharWeight where
  arbitrary = CharWeight <$> arbitrary <*> arbitrary

instance Arbitrary StringTrie where
  arbitrary = choose (0, 10) >>= getArb

getArb :: Int -> Gen StringTrie
getArb n = 
  let n2 = n `div` 2
      children :: Gen [StringTrie]
      children = case n2 of
        0 -> return []
        _ -> mapM (\_ -> getArb n2) [0..n]
  in TrieNode <$> arbitrary <*> children

-- Helper Methods --

toString :: Functor f => f CharWeight -> f Char
toString = fmap fromCharWeight

prefixesOf :: (Eq a, Traversable t) => t [a] -> t [a] -> Bool
prefixesOf containee container =
  and $ fmap f containee
  where
    f prefix = or $ fmap (isPrefix prefix) container

isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix xs ys = and $ zipWith (==) xs ys

insertN :: Int -> String -> [StringTrie] -> [StringTrie]
insertN n s trie = foldl (\t _ -> insert s t) trie [1..n]

none :: Foldable t => (a -> Bool) -> t a -> Bool
none = (not .) . any

type NonEmptyStr = NonEmptyList Char

-- Tests --

prop_lookupNewEntry :: String -> [StringTrie] -> Property
prop_lookupNewEntry s ts = 
  length s > 0 ==>
  let ts' = insert s ts
  in toString (lookup s ts') == s


prop_lookupAllNewEntries :: String -> [String] -> [StringTrie] -> Property
prop_lookupAllNewEntries prefix suffixes trie =
  length prefix > 0 &&
  all (\s -> length s > 0) suffixes
  ==>
  let fullStrings = map (prefix++) suffixes
      trie' = foldl (flip insert) trie fullStrings
      res = allMatches prefix trie'
      resStrings = fmap toString res
  in prefixesOf fullStrings resStrings

prop_relevanceMatters :: NonEmptyStr -> [NonEmptyStr] -> Property
prop_relevanceMatters (NonEmpty prefix) neSuffixes =
  -- Probably a nicer way, check all elems start with different chars
  none (\x -> any (isPrefix x) suffixes) suffixes 
  ==>
  all (uncurry checkLookup) (zip ts suffixes)
  where
    suffixes = map (\(NonEmpty x) -> x) neSuffixes
    xs = map (prefix++) suffixes

    checkLookup :: [StringTrie] -> String -> Bool
    checkLookup trie suffix = toString (lookup prefix trie) == prefix ++ suffix

    ts :: [[StringTrie]]
    -- We drop the empty list in the first position
    ts = tail $ scanl (\trie (i, x) -> insertN i x trie) [] (zip [1..] xs)

prop_relevanceCommutes :: NonEmptyStr -> String -> String -> Int -> Int -> Bool
prop_relevanceCommutes (NonEmpty prefix) a b x y = 
  (lookup prefix $ insertA $ insertB [])
  ==
  (lookup prefix $ insertB $ insertA [])
  where
    insertA = insertN x (prefix ++ a)
    insertB = insertN y (prefix ++ b)

prop_relevanceAssociates :: NonEmptyStr -> String -> String -> String -> Int -> Int -> Int -> Bool
prop_relevanceAssociates (NonEmpty prefix) a b c x y z =
  (lookup prefix $ (insertA . insertB) (insertC []))
  ==
  (lookup prefix $ insertA ((insertB . insertC) []))
  where
    insertA = insertN x (prefix ++ a)
    insertB = insertN y (prefix ++ b)
    insertC = insertN z (prefix ++ c)
