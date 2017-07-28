module Draw where

import StringTries
import Trie
import TrieState
import Complete

import Data.List.Zipper
import System.Directory
import System.Environment
import System.Cmd
import System.Console.ANSI
import System.IO

prePrompt :: IO String
prePrompt = do 
  pwd <- getCurrentDirectory
  return $ parseFilename (show pwd)  ++ ">>> "

drawCompletion :: FishyState -> IO ()
drawCompletion state = do
  let s :: String
      s = toList p
      p@(Zip pl pr) = getPrompt state
  currentDir <- getCurrentDirectory

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
  let (completion, color) = fishyComplete state currentDir
  setSGR [SetColor Foreground Vivid color]
  putStr $ drop (length s) completion
  setSGR [Reset]

  -- Set cursor location to the position from the prompt zipper
  -- (Zipper centre used to represent cursor pos in input)
  setCursorColumn $ length prePromptS + length pl
  hFlush stdout
