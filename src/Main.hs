module Main where

import Trie
import System.Directory
import System.Console.ANSI

stripQuotes :: String -> String
stripQuotes = filter (\x -> x /= '"')

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  putStrLn $ show currentDir
  files <- listDirectory currentDir
  putStrLn $ concatMap (((++)"\n") . stripQuotes . show) files
  let tries = buildTries files
  completeIO tries


buildTries :: [FilePath] -> [Trie CharWeight]
buildTries files = foldr insertCW [] (fmap (stripQuotes . show) files)

complete :: String -> [Trie CharWeight] -> String
complete s ts = fmap fromCharWeight $ lookupCW s ts

completeIO :: [Trie CharWeight] -> IO ()
completeIO tries = do
  putStr ">>> "
  prefix <- getLine
  putStr ('\n':prefix)
  setSGR [SetColor Foreground Vivid Red]
  putStrLn $ drop (length prefix) (complete prefix tries)
  setSGR [Reset]
