{-# LANGUAGE ForeignFunctionInterface #-}

module Update where

import Trie
import TrieState
import Draw

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Data.Char (chr, ord)
import System.Console.ANSI
import Foreign.C.Types
import System.Cmd

data CommandInput = Text String | Run | Exit | Execute String

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
  let s = getString state
  lift $ drawCompletion state
  c <- lift getHiddenChar
  case matchChar state c of 
    Text str -> do
      lift $ setCursorColumn 0
      lift clearFromCursorToLineEnd
      put state {getString = str}
      return False
    Exit -> return True
    Run -> do
      lift $ putStr "\n"
      exitcode <- lift $ system s
      lift $ putStr "\n"
      let history = insertCW s $ getHistoryTries state
      put $ state {getHistoryTries = history, getString = ""}
      sss <- get
      lift $ saveState sss
      return False
    Execute command -> lift $ do
      exitcode <- system command
      return False

matchChar :: CompleteState -> Char -> CommandInput
matchChar state c = case ord c of
  10  -> Run                          -- Newline
  13  -> Run                          -- Newline (Windows)
  8   -> Text $ take (length s - 1) s -- Backspace (Windows)
  127 -> Text $ take (length s - 1) s -- Backspace (Windows Ctr+backspace)
  6   -> Text $ complete state         -- Complete (Windows Ctr+F form feed)
  12  -> Execute "cls"                -- Clear screen (Ctr+L)
  3   -> Exit                         -- Exit (Windows Ctr+C)
  4   -> Exit                         -- EOF (Windows Ctr+D)
  x   -> Text $ s ++ [c]
  where s = getString state
  
