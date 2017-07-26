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

-- Complete a prefix
complete :: FishyState -> String
complete state = case length splitS of
  -- Don't do anything for empty string
  0 -> ""

  -- For a single 'word' use all tries
  1 -> defOr s $ completeAll

  -- For multiple 'words'
  _ -> if length completeAll > length s
    -- Prefer history, otherwise complete on filenames
    then completeAll
    -- Otherwise use files in current directory
    else let x = last splitS in 
      defOr x $ s ++ drop (length x) (fromTries (getFileTries state) x)
  where 
    splitS :: [String]
    splitS = splitOn " " s
    s :: String
    s = toList $ getPrompt state
    defOr s f = if s == "" then "" else f
    completeAll = fromTries (bigTrie state) s

bigTrie :: FishyState -> [Trie CharWeight]
bigTrie state = getFileTries state ++ getHistoryTries state ++ getPathTries state

prePrompt :: IO String
prePrompt = do 
  pwd <- getCurrentDirectory
  return $ parseFilename (show pwd)  ++ ">>> "

drawCompletion :: FishyState -> IO ()
drawCompletion state = do
  let s :: String
      s = toList p
      p@(Zip pl pr) = getPrompt state
      ts = getFileTries state

  -- Fetch prePrompt
  prePromptS <- prePrompt
  let drawstr = prePromptS ++ s
  
  -- Set cursor to start of line
  setCursorColumn 0
  -- Draw preprompt and user input
  putStr drawstr
  -- Set cursor to end of what we just wrote and clear
  setCursorColumn $ length drawstr
  clearFromCursorToLineEnd

  -- Set color and draw completion
  setSGR [SetColor Foreground Vivid Red]
  putStr $ drop (length s) (complete state)
  setSGR [Reset]

  -- Set cursor location to the position from the prompt zipper
  -- (Zipper centre used to represent cursor pos in input)
  setCursorColumn $ length prePromptS + length pl
  hFlush stdout
