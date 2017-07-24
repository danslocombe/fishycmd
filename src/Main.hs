{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Trie
import TrieState

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Data.Char (chr, ord)
import Data.Semigroup ((<>))
import Foreign.C.Types
import System.Cmd
import System.Console.ANSI
import System.Directory
import System.Environment
import System.IO
import Options.Applicative

data FishyOptions = FishyOptions
  { clearHistory :: Bool }

parseOptions :: Parser FishyOptions
parseOptions = FishyOptions
  <$> switch
     ( long "clear-history"
    <> short 'c'
    <> help "Clear command history" )

stripQuotes :: String -> String
stripQuotes = filter (/= '"')

stripDoubleBackslash :: String -> String
stripDoubleBackslash [] = []
stripDoubleBackslash ('\\':'\\':xs) = '\\' : stripDoubleBackslash xs
stripDoubleBackslash (x:xs) = x : stripDoubleBackslash xs

parseFilename :: String -> String
parseFilename = stripQuotes . stripDoubleBackslash

getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt

repeaty :: CompleteState -> IO ()
repeaty state = do
  (res, state') <- runStateT updateIOState state
  if res then return () else repeaty state'

main :: IO ()
main = do
  options <- execParser opts
  sp <- statePath
  createDirectoryIfMissing True sp
  putStrLn entryString
  currentDir <- getCurrentDirectory
  files <- listDirectory currentDir
  -- putStrLn $ concatMap (((++)"\n") . stripQuotes . show) files
  let fileTries = buildTries files
  complete <- if clearHistory options 
    then return $ CompleteState [] fileTries ""
    else loadState fileTries
  repeaty complete

opts :: ParserInfo FishyOptions
opts = info (parseOptions <**> helper)
  ( fullDesc
  <> progDesc "Run the shell"
  <> header "FishyCMD - A wrapper for CMD" )

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
  
bigTrie :: CompleteState -> [Trie CharWeight]
bigTrie state = getFileTries state ++ getHistoryTries state

buildTries :: [FilePath] -> [Trie CharWeight]
buildTries files = foldr insertCW [] (fmap (parseFilename . show) files)

complete :: CompleteState -> String
complete state = if length s > 0 
  then fmap fromCharWeight $ lookupCW s ts
  else ""
  where s = getString state
        ts = bigTrie state

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

entryString :: String
-- entryString = "FishyCMD"
entryString = "\n\
\ ______________       ______         ______________  __________ \n\
\ ___  ____/__(_)_________  /______  ___  ____/__   |/  /__  __ \\\n\
\ __  /_   __  /__  ___/_  __ \\_  / / /  /    __  /|_/ /__  / / /\n\
\ _  __/   _  / _(__  )_  / / /  /_/ // /___  _  /  / / _  /_/ / \n\
\ /_/      /_/  /____/ /_/ /_/_\\__, / \\____/  /_/  /_/  /_____/  \n\
\                           /____/                             \n\
\ \n                  2ks Oneweek Hackathon 2017 - Dan Slocombe\n"
