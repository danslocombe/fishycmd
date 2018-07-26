{-# LANGUAGE FlexibleInstances #-}

module Test.Trie where

import Prelude hiding (lookup)
import Test.QuickCheck
import Control.Monad
import Data.List (nub)

import Complete.Trie
import Complete.String

runTests :: IO ()
runTests = do
  putStrLn "lookup new entry"
  quickCheck prop_lookupNewEntry
  putStrLn "lookup all new entries"
  quickCheck prop_lookupAllNewEntries
  putStrLn "check relevance matters"
  quickCheck prop_relevanceMatters
  putStrLn "check relevance commutes"
  quickCheck prop_relevanceCommutes
  putStrLn "check relevance associates"
  quickCheck prop_relevanceAssociates
  putStrLn "check abc abcd abcde"
  quickCheck prop_willGivePrefixes
  putStrLn "check trie will suggest the whole prefix"
  quickCheck prop_suggestPrefix
  return () 

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
  in TrieNode <$> arbitrary <*> children <*> arbitrary

-- Helper Methods --

--toString :: Functor f => f CharWeight -> f Char
--toString = fmap fromCharWeight

prefixesOf :: (Eq a, Traversable t) => t [a] -> t [a] -> Bool
prefixesOf containee container =
  and $ fmap f containee
  where
    f prefix = or $ fmap (isPrefix prefix) container

isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix xs ys = and $ zipWith (==) xs ys

insertN :: Int -> String -> [StringTrie] -> [StringTrie]
insertN n s trie = foldl (\t _ -> insertTrie s t) trie [1..n]

none :: Foldable t => (a -> Bool) -> t a -> Bool
none = (not .) . any

type NonEmptyStr = NonEmptyList Char

-- Tests --

prop_lookupNewEntry :: String -> [StringTrie] -> Property
prop_lookupNewEntry s ts = 
  length s > 0 ==>
  let ts' = insertTrie s ts
  in cwToString (lookupTrie s ts') == s


prop_lookupAllNewEntries :: NonEmptyStr -> [NonEmptyStr] -> [StringTrie] -> Bool
prop_lookupAllNewEntries (NonEmpty prefix) nonEmptysuffixes trie =
  let suffixes = map (\(NonEmpty s) -> s) nonEmptysuffixes
      fullStrings = map (prefix++) suffixes
      trie' = foldl (flip insertTrie) trie fullStrings
      res = allTrieMatches prefix trie'
      resStrings = fmap cwToString res
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
    checkLookup trie suffix = cwToString (lookupTrie prefix trie) == prefix ++ suffix

    ts :: [[StringTrie]]
    -- We drop the empty list in the first position
    ts = tail $ scanl (\trie (i, x) -> insertN i x trie) [] (zip [1..] xs)

prop_relevanceCommutes :: NonEmptyStr -> String -> String -> Int -> Int -> Property
prop_relevanceCommutes (NonEmpty prefix) a b x y = 
  x > y ==> 
    (lookupTrie prefix $ insertA $ insertB [])
    ==
    (lookupTrie prefix $ insertB $ insertA [])
  where
    insertA = insertN x (prefix ++ a)
    insertB = insertN y (prefix ++ b)

prop_relevanceAssociates :: NonEmptyStr -> String -> String -> String -> Int -> Int -> Int -> Bool
prop_relevanceAssociates (NonEmpty prefix) a b c x y z =
  (lookupTrie prefix $ (insertA . insertB) (insertC []))
  ==
  (lookupTrie prefix $ insertA ((insertB . insertC) []))
  where
    insertA = insertN x (prefix ++ a)
    insertB = insertN y (prefix ++ b)
    insertC = insertN z (prefix ++ c)

-- If we have a prefix abc and a trie with weights (abcd, 10) (abcde, 1) then we 
-- should give abcd not abcde
prop_willGivePrefixes :: String -> NonEmptyStr -> NonEmptyStr -> Int -> Int -> Property
prop_willGivePrefixes s0 (NonEmpty s1) (NonEmpty s2) w0 w1 =
  w1 > w0 && w0 > 0 ==>
    (cwToString $ lookupTrie prefix t) == abcd
    where
      prefix = s0
      abcd = prefix ++ s1
      abcde = abcd ++ s2

      t0 = insertN w0 abcde []
      t  = insertN w1 abcd  t0

prop_suggestPrefix :: [StringTrie] -> NonEmptyStr -> Bool
prop_suggestPrefix st (NonEmpty p) = 
  p == (cwToString $ (lookupTrie p $ insertTrie p []))
