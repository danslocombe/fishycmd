module FileTries where

import Trie

import System.Directory
import System.Environment

stripQuotes :: String -> String
stripQuotes = filter (/= '"')

stripDoubleBackslash :: String -> String
stripDoubleBackslash [] = []
stripDoubleBackslash ('\\':'\\':xs) = '\\' : stripDoubleBackslash xs
stripDoubleBackslash (x:xs) = x : stripDoubleBackslash xs

parseFilename :: String -> String
parseFilename = stripQuotes . stripDoubleBackslash

buildTries :: [String] -> [Trie CharWeight]
buildTries files = foldr insertCW [] (fmap (parseFilename . show) files)
