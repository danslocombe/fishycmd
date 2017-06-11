{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Trie
import System.Directory
import System.Console.ANSI
import Data.Char (chr, ord)
import Foreign.C.Types
import System.Cmd

stripQuotes :: String -> String
stripQuotes = filter (\x -> x /= '"')

getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  putStrLn $ show currentDir
  files <- listDirectory currentDir
  putStrLn $ concatMap (((++)"\n") . stripQuotes . show) files
  let tries = buildTries files
  updateIO "" tries
  return ()

data CommandInput = Text String | Run | Exit

updateIO :: String -> [Trie CharWeight] -> IO ()
updateIO s ts = do
  drawCompletion s ts
  c <- getHiddenChar
  -- putStrLn $ '\n' : (show $ ord c)
  match <- return $ case ord c of
    10  -> Run                          -- Newline
    13  -> Run                          -- Newline (Windows)
    8   -> Text $ take (length s - 1) s -- Backspace (Windows)
    127 -> Text $ take (length s - 1) s -- Backspace (Windows Ctr+backspace)
    6   -> Text $ complete s ts         -- Complete (Windows Ctr+F form feed)
    12  -> Text ""                      -- Clear screen (Ctr+L)
    3   -> Exit                         -- Exit (Windows Ctr+C)
    4   -> Exit                         -- EOF (Windows Ctr+D)
    x   -> Text $ s ++ [c]
  
  case match of 
    Text str -> do
      setCursorColumn 0
      clearFromCursorToLineEnd
      updateIO str ts
    Exit -> return ()
    Run -> do
      exitcode <- system s
      updateIO "" ts

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
