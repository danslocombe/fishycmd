{-# LANGUAGE ForeignFunctionInterface #-}

module Update where

import Trie
import TrieState
import Draw

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Data.Char (chr, ord)
import Data.List.Zipper
import System.Console.ANSI
import Foreign.C.Types
import System.Cmd

data CommandInput = Text (Zipper Char)
                  | Run
                  | Exit
                  | Execute String
                  | PrepControlChar

getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt

repeaty :: CompleteState -> IO ()
repeaty state = do
  (res, state') <- runStateT updateIOState state
  if res then return () else repeaty state'


updateIOState :: StateT CompleteState IO Bool
updateIOState = do
  state <- get
  let p = getPrompt state
  lift $ drawCompletion state
  c <- lift getHiddenChar
  -- lift $ putStrLn $ show $ ord c
  case matchChar state c of 
    Text prompt -> do
      lift $ setCursorColumn 0
      lift clearFromCursorToLineEnd
      put state {getPrompt = prompt, getControlPrepped = False}
      return False
    Exit -> return True
    Run -> do
      lift $ putStr "\n"
      let s = toList p
      exitcode <- lift $ system s
      lift $ putStr "\n"
      let history = insertCW s $ getHistoryTries state
      let state' = state {getHistoryTries = history, getPrompt = empty}
      put state'
      lift $ saveState state
      return False
    Execute command -> lift $ do
      exitcode <- system command
      return False
    PrepControlChar -> do
      put state {getControlPrepped = True}
      lift $ return False

matchChar :: CompleteState -> Char -> CommandInput
matchChar state c = case ord c of
  10  -> Run                          -- Newline
  13  -> Run                          -- Newline (Windows)
  8   -> Text $ Zip (drop 1 s) s' -- Backspace (Windows)
  127 -> Text $ Zip (drop 1 s) s' -- Backspace (Windows Ctr+backspace)
  6   -> Text $ Zip (reverse (complete state)) []        -- Complete (Windows Ctr+F form feed)
  12  -> Execute "cls"                -- Clear screen (Ctr+L)
  3   -> Exit                         -- Exit (Windows Ctr+C)
  4   -> Exit                         -- EOF (Windows Ctr+D)
  224 -> PrepControlChar
  75  -> ifControlPrepped $ left p
  77  -> ifControlPrepped $ right p
  x   -> Text $ push c p
  where p@(Zip s s') = getPrompt state
        ifControlPrepped r = Text $
          if getControlPrepped state then r else push c p
