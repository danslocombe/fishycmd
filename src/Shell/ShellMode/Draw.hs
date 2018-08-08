module Shell.ShellMode.Draw (prePrompt, drawCompletion) where

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
import Data.List.Split
import Data.List (intersperse)

prePrompt :: IO String
prePrompt = do 
  pwd <- getCurrentDirectory
  let processed = processPromptSafe $ parseFilename $ show pwd
      pre = case processed of
        Just x -> x
        Nothing -> ">°))))<  "
  return $ pre ++ ">>> "

promptTargetLength = 25

processPromptSafe :: String -> Maybe String
processPromptSafe p = do
  let chopped = splitOn  "\\" p

  -- Abort on length 0
  n <- case length chopped of 
    0 -> Nothing
    x -> Just x

  -- Well defined as |chopped| > 0
  let chopped' = init chopped
      cd = last chopped

      (half0, half1) = splitAt (n `div` 2) chopped'

      removeVowels :: String -> Maybe String
      removeVowels [] = Nothing
      removeVowels (x:xs) = Just $ x : (filter (\x -> not $ x `elem` "aeiouAEIOU") xs)

      safeFirst [] = Nothing
      safeFirst (x:xs) = Just [x]

      (+^+) :: Monad m => m [a] -> m [a] -> m [a]
      (+^+) = liftM2 (++)

      toTry :: [Maybe [String]]
      toTry =
        [ (sequence $ removeVowels <$> half0) +^+ (Just half1)
        , sequence $ removeVowels <$> chopped'
        , (sequence $ safeFirst <$> half0) +^+ (sequence $ removeVowels <$> half1)
        , sequence $ safeFirst <$> chopped'
        ]

  toTry' <- sequence toTry
  let applied = map (concat . intersperse "\\") toTry' :: [String]

  let base = case dropWhile (\x -> length x > promptTargetLength) applied of
        [] -> concat $ intersperse "\\" $ last toTry'
        x:_ -> x

  return $ base ++ "\\" ++ cd
   

processPrompt :: String -> String
processPrompt p = ret ++ "\\" ++ cd
  where
    chopped = splitOn "\\" p
    cd = last chopped
    chopped' = init chopped
    n = length chopped'
    (half0, half1) = splitAt (n `div` 2) chopped'

    removeVowels [] = []
    removeVowels (x:xs) = x : (filter (\x -> not $ x `elem` "aeiouAEIOU") xs)
    toTry =
      [(removeVowels <$> half0) ++ half1,
       removeVowels <$> chopped',
       (return . head <$> half0) ++ half1,
       (return . head <$> half0) ++ (removeVowels <$> half1),
       (return . head <$> half0) ++ (return . head <$> half1)
       ]
    applied = map (concat . intersperse "\\") toTry :: [String]
    ret = head $ (dropWhile (\x -> length x > promptTargetLength) applied)             ++ last toTry


drawCompletion' :: Int -> [(String, Color)] -> IO ()
drawCompletion' = undefined

drawCompletion :: Int -> String -> Zipper Char -> String -> Color -> IO ()
drawCompletion lastHeight preprompt p@(Zip pl pr) completion color = do
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
