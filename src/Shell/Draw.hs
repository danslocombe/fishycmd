module Shell.Draw (prePrompt, drawCompletion) where

import Complete
import Complete.String
import Shell.State
import Shell.Helpers

import Control.Monad
import Data.List.Zipper
import System.Directory
import System.Environment
import System.Console.ANSI
import System.Console.Terminal.Size
import System.IO

prePrompt :: IO String
prePrompt = do 
  pwd <- getCurrentDirectory
  return $ parseFilename (show pwd)  ++ ">>> "

drawCompletion' :: Int -> [(String, Color)] -> IO ()
drawCompletion' = undefined

drawCompletion :: Int -> String -> Zipper Char -> StringCompletion -> Color -> IO ()
drawCompletion lastHeight preprompt p@(Zip pl pr) (Completion completion _) color = do
  let s :: String
      s = toList p
  Just (Window _ ww) <- size
  currentDir <- getCurrentDirectory

  let drawstr = preprompt ++ s

  --  Clear previous
  replicateM_ (lastHeight - 1) $ do
    setCursorColumn 0
    clearFromCursorToLineEnd
    cursorUp 1

  setCursorColumn 0
  clearFromCursorToLineEnd
  
  let thisLength = length preprompt + max (length s) (length completion)
      thisHeight = 1 + (thisLength `div` ww)

  lastHeight > thisHeight
    ?-> cursorDown 1
 
  -- Draw preprompt and user input
  putStr drawstr

  let completionOnly = drop (length s) completion
      completionHeight = length drawstr + length completionOnly

  setSGR [SetColor Foreground Vivid color]
  putStr completionOnly
  setSGR [Reset]

  -- Set cursor location to the position from the prompt zipper
  -- (Zipper centre used to represent cursor pos in input)
  let len = length preprompt + length pl
  -- putStrLn $ show thisHeight
  -- putStrLn $ show (len `div` ww)
  cursorUp $ thisHeight - 1
  cursorDown $ len `div` ww
  -- cursorUp $ thisHeight - ((len `div` ww) + 1)
  setCursorColumn $ len `mod` ww
  hFlush stdout

  -- cursorDown 1
  -- putStrLn  drawstr
