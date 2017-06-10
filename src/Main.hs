module Main where

import Trie
import System.Directory

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  putStrLn $ show currentDir
  files <- listDirectory currentDir
  putStrLn $ concatMap show files
  let tries = foldr (insertCW) [] (fmap show files)
      x = fmap fromCharWeight $ lookupCW "\"s" tries
  -- putStrLn $ show tries
  putStrLn x
  putStrLn "hello world"
