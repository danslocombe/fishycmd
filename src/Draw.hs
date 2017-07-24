module Draw where

import Trie
import TrieState

import System.Directory
import System.Environment
import System.Cmd
import System.Console.ANSI
import System.IO

stripQuotes :: String -> String
stripQuotes = filter (/= '"')

stripDoubleBackslash :: String -> String
stripDoubleBackslash [] = []
stripDoubleBackslash ('\\':'\\':xs) = '\\' : stripDoubleBackslash xs
stripDoubleBackslash (x:xs) = x : stripDoubleBackslash xs

parseFilename :: String -> String
parseFilename = stripQuotes . stripDoubleBackslash

complete :: CompleteState -> String
complete state = if length s > 0 
  then fmap fromCharWeight $ lookupCW s ts
  else ""
  where s = getString state
        ts = bigTrie state

bigTrie :: CompleteState -> [Trie CharWeight]
bigTrie state = getFileTries state ++ getHistoryTries state

prompt :: IO String
prompt = do 
  pwd <- getCurrentDirectory
  return $ parseFilename (show pwd)  ++ ">>> "

drawCompletion :: CompleteState -> IO ()
drawCompletion state = do
  let s = getString state
      ts = getFileTries state
  promptS <- prompt
  let drawstr = promptS ++ s
  putStr $ '\r' : drawstr
  setCursorColumn $ length drawstr
  clearFromCursorToLineEnd
  setSGR [SetColor Foreground Vivid Red]
  putStr $ drop (length s) (complete state)
  setSGR [Reset]
  setCursorColumn $ length drawstr
  hFlush stdout

