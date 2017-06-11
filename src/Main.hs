{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Trie
import System.Directory
import System.Console.ANSI
import System.IO
import Data.Char (chr, ord)
import Foreign.C.Types
import System.Cmd
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class

stripQuotes :: String -> String
stripQuotes = filter (\x -> x /= '"')

getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt

entryString :: String
entryString = "Fishy CMD"

repeaty :: CompleteState -> IO ()
repeaty state = do
  (res, state') <- runStateT updateIOState state
  if res then return () else repeaty state'

data CompleteState = CompleteState 
  { getHistoryTries :: [Trie CharWeight]
  , getFileTries    :: [Trie CharWeight]
  , getString       :: String
  } deriving (Show)

main :: IO ()
main = do
  putStrLn entryString
  currentDir <- getCurrentDirectory
  files <- listDirectory currentDir
  -- putStrLn $ concatMap (((++)"\n") . stripQuotes . show) files
  let tries = buildTries files
  repeaty $ CompleteState [] tries ""

data CommandInput = Text String | Run | Exit | Execute String

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
      exitcode <- lift $ system s
      let history = insertCW s $ getHistoryTries state
      put $ state {getHistoryTries = history, getString = ""}
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
  
bigTrie :: CompleteState -> [Trie CharWeight]
bigTrie state = getFileTries state ++ getHistoryTries state

buildTries :: [FilePath] -> [Trie CharWeight]
buildTries files = foldr insertCW [] (fmap (stripQuotes . show) files)

complete :: CompleteState -> String
-- For now
complete state = fmap fromCharWeight $ lookupCW s ts
  where s = getString state
        ts = bigTrie state

prompt :: String
prompt = ">>> "

drawCompletion :: CompleteState -> IO ()
drawCompletion state = do
  let s = getString state
      ts = getFileTries state
  let drawstr = prompt ++ s
  putStr $ '\r' : drawstr
  setCursorColumn $ length drawstr
  clearFromCursorToLineEnd
  setSGR [SetColor Foreground Vivid Red]
  putStr $ drop (length s) (complete state)
  setSGR [Reset]
  setCursorColumn $ length drawstr
  hFlush stdout
