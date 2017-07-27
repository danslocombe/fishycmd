{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module FileCompleter where

import Completer

import Data.Maybe (listToMaybe, fromMaybe)
import Data.List.Split (splitOn)
import Text.Regex.Posix ((=~))
import Safe
import System.Directory

data FileCompleter = FileCompleter String [String] deriving (Show)

createFileCompleter :: FileCompleter -> String -> IO FileCompleter
createFileCompleter prev prefix = do
  let ps = splitOn " " prefix
      targetDir = if length ps > 1 then last ps else prefix
      tdSplit = concatMap (splitOn "\\") $ splitOn "/" targetDir
      n = length tdSplit
      -- secondLast = last $ take (n - 1) tdSplit
      targetDir' = if n > 1 
        then concatMap (++"\\") $ take (n - 1) tdSplit 
        else if targetDir /= "" && targetDir =~ ".+(/|\\\\)$" then targetDir else ""
  fileCompleterFromRelDir prev targetDir'

fileCompleterFromRelDir :: FileCompleter -> String -> IO FileCompleter
fileCompleterFromRelDir prev@(FileCompleter cachedDir _) prefix = do
  exists <- doesPathExist prefix
  dir <- if exists
           then return prefix
           else do
             cd <- getCurrentDirectory
             exists' <- doesPathExist (cd ++ prefix)
             if exists' then return $ cd ++ prefix
             else return cd
  -- files <- listDirectory dir
  -- mapM putStrLn files
  if cachedDir == dir
  -- Don't update, use prev
  then return prev
  -- Rebuild from dir
  else fileCompleterFromDir dir
  
fileCompleterFromDir :: String -> IO FileCompleter
fileCompleterFromDir dir = do
  -- putStrLn $ "Creating completer for " ++ dir
  files <- listDirectory dir
  -- Hacky
  let withCd  = (dir++) <$> files
      withCd' = backslashToForward <$> withCd
  return $ FileCompleter dir (files ++ withCd ++ withCd')

backslashToForward = fmap (\c -> case c of
  '\\' -> '/'
  x -> x)

instance Completer FileCompleter where
  type CompleteType FileCompleter = Char
  complete (FileCompleter _ fs) prefix = fromMaybe "" $ listToMaybe $ candidates
    where candidates = filter (startsWith prefix) fs

startsWith :: String -> String -> Bool
startsWith xs ys = and $ zipWith (==) xs ys
