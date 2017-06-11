module Main where

import Trie
import System.Directory
import System.Console.ANSI
import Data.Char (chr, ord)

stripQuotes :: String -> String
stripQuotes = filter (\x -> x /= '"')

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  putStrLn $ show currentDir
  files <- listDirectory currentDir
  putStrLn $ concatMap (((++)"\n") . stripQuotes . show) files
  let tries = buildTries files
  updateIO "" tries
  return ()

updateIO :: String -> [Trie CharWeight] -> IO ()
updateIO s ts = do
  drawCompletion s ts
  c <- getChar
  str <- return $ case ord c of
    10 -> ""                     -- Newline
    127 -> take (length s - 1) s -- Backspace
    6 -> complete s ts           -- Control+F (form feed)
    x -> s ++ [c]
  setCursorColumn 0
  clearFromCursorToLineEnd
  updateIO str ts

buildTries :: [FilePath] -> [Trie CharWeight]
buildTries files = foldr insertCW [] (fmap (stripQuotes . show) files)

complete :: String -> [Trie CharWeight] -> String
complete s ts = fmap fromCharWeight $ lookupCW s ts

prompt :: String
prompt = ">>> "

drawCompletion :: String -> [Trie CharWeight] -> IO ()
drawCompletion s tries = do
  let drawstr = prompt ++ s
  putStr $ '\r' : drawstr
  setCursorColumn $ length drawstr
  clearFromCursorToLineEnd
  setSGR [SetColor Foreground Vivid Red]
  putStr $ drop (length s) (complete s tries)
  setSGR [Reset]
  setCursorColumn $ length drawstr
