module Draw where

import FileTries
import Trie
import TrieState

import Data.List.Zipper
import System.Directory
import System.Environment
import System.Cmd
import System.Console.ANSI
import System.IO
import Data.List.Split

fromTries :: [Trie CharWeight] -> String -> String
fromTries ts s = fmap fromCharWeight $ lookupCW s ts

complete :: CompleteState -> String
complete state = case length splitS of
  0     -> ""
  1     -> defOr s $ fromTries (bigTrie state) s
  _     -> let x = last splitS in 
    defOr x $ s ++ drop (length x) (fromTries (getFileTries state) x)
  where 
    splitS :: [String]
    splitS = splitOn " " s
    s :: String
    s = toList $ getPrompt state
    defOr s f = if s == "" then "" else f

bigTrie :: CompleteState -> [Trie CharWeight]
bigTrie state = getFileTries state ++ getHistoryTries state ++ getPathTries state

prePrompt :: IO String
prePrompt = do 
  pwd <- getCurrentDirectory
  return $ parseFilename (show pwd)  ++ ">>> "

drawCompletion :: CompleteState -> IO ()
drawCompletion state = do
  let s :: String
      s = toList p
      p@(Zip pl pr) = getPrompt state
      ts = getFileTries state
  prePromptS <- prePrompt
  let drawstr = prePromptS ++ s
  setCursorColumn 0
  putStr drawstr
  setCursorColumn $ length drawstr
  clearFromCursorToLineEnd
  setSGR [SetColor Foreground Vivid Red]
  putStr $ drop (length s) (complete state)
  setSGR [Reset]
  setCursorColumn $ length prePromptS + length pl
  hFlush stdout
