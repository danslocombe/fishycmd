{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Trie
import System.Directory
import System.Console.ANSI
import Data.Char (chr, ord)
import Foreign.C.Types

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

updateIO :: String -> [Trie CharWeight] -> IO ()
updateIO s ts = do
  drawCompletion s ts
  c <- getHiddenChar
  -- putStrLn $ '\n' : (show $ ord c)
  match <- return $ case ord c of
    10  -> Just ""                      -- Newline
    13  -> Just ""                      -- Newline (Windows)
    8   -> Just $ take (length s - 1) s -- Backspace (Windows)
    127 -> Just $ take (length s - 1) s -- Backspace (Windows Ctr+backspace)
    6   -> Just $ complete s ts         -- Complete (Windows Ctr+F form feed)
    12  -> Just ""                      -- Clear screen (Ctr+L)
    3   -> Nothing                      -- Exit (Windows Ctr+C)
    4   -> Nothing                      -- EOF (Windows Ctr+D)
    x   -> Just $ s ++ [c]
  
  case match of 
    Just str -> do
      setCursorColumn 0
      clearFromCursorToLineEnd
      updateIO str ts
    Nothing -> return ()

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
