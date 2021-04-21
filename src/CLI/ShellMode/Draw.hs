module CLI.ShellMode.Draw (prePrompt, generatePrePromptSafe, possibleShortenedPrefixes, drawCompletion) where

import Complete.String
import CLI.Helpers

import Control.Monad
import Data.List.Zipper
import System.Directory
import System.Console.ANSI
import System.Console.Terminal.Size
import System.IO
import Data.List.Split
import Data.List (intersperse)

prePrompt :: IO String
prePrompt = do 
  pwd <- getCurrentDirectory
  let processed = generatePrePromptSafe $ parseFilename $ show pwd
      pre = case processed of
        Just x -> x
        Nothing -> ">°))))<  "
  return $ pre ++ ">>> "

promptTargetLength :: Int
promptTargetLength = 40

removeVowels :: String -> Maybe String
removeVowels [] = Nothing
removeVowels (x:xs) = Just $ x : (filter (\x -> not $ x `elem` "aeiouAEIOU") xs)

-- Build up a list of preprompts to try until we get one under the character limit
-- EG
-- C:\Usrs\dsloom\Folder
-- C:\U\d\Folder
possibleShortenedPrefixes :: [String] -> [Maybe [String]]
possibleShortenedPrefixes chopped =
      -- Half de-voweled, half normal
      [ (sequence $ removeVowels <$> half0) +^+ (Just half1)
      -- All de-voweled
      , sequence $ removeVowels <$> chopped'
      -- Half with just first letter, half de-voweled
      , (sequence $ safeFirst <$> half0) +^+ (sequence $ removeVowels <$> half1)
      -- All first letter
      , sequence $ safeFirst <$> chopped'
      ]
  where
    -- Assume n > 0
    n = length chopped
    chopped' = init chopped
    (half0, half1) = splitAt (n `div` 2) chopped'

-- Generate a 'pre prompt' by compressing the current path down
generatePrePromptSafe :: String -> Maybe String
generatePrePromptSafe p = do
  let chopped = splitOn  "\\" p

  -- Abort on length 0
  _ <- case length chopped of 
    0 -> Nothing
    _ -> Just ()

  toTry <- sequence $ possibleShortenedPrefixes chopped
  let applied = map (concat . intersperse "\\") toTry :: [String]

  let base = case dropWhile (\x -> length x > promptTargetLength) applied of
        [] -> concat $ intersperse "\\" $ last toTry
        x:_ -> x
  
  let cd = last chopped

  return $ base ++ "\\" ++ cd

drawCompletion :: Int -> String -> Zipper Char -> String -> Color -> IO ()
drawCompletion lastHeight preprompt p@(Zip pl pr) completion color = do
  let s :: String
      s = toList p
  Just (Window _ ww) <- size

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

  when (lastHeight > thisHeight) $ cursorDown 1
 
  -- Draw preprompt and user input
  putStr drawstr

  let completionOnly = drop (length s) completion

  setSGR [SetColor Foreground Vivid color]
  putStr completionOnly
  setSGR [Reset]

  -- Set cursor location to the position from the prompt zipper
  -- (Zipper centre used to represent cursor pos in input)
  let len = length preprompt + length pl
  cursorUp $ thisHeight - 1
  cursorDown $ len `div` ww
  setCursorColumn $ len `mod` ww
  hFlush stdout

  --putStrLn $ show thisHeight
