{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}

module FileCompleter where

import Completer

import Data.Maybe (listToMaybe, fromMaybe)
import Data.List.Split (splitOn)
import Text.Regex.Posix ((=~))
import Safe
import System.Directory

data FileCompleter = FileCompleter [String] deriving (Show)

createFileCompleter :: String -> String -> IO FileCompleter
createFileCompleter prevDir prefix = do
  let ps = splitOn " " prefix
      input = if length ps > 1 then last ps else prefix
      inputSplit = concatMap (splitOn "\\") $ splitOn "/" input
      n = length inputSplit
      secondLast = last $ take (n - 1) inputSplit
      input' = if n > 1 
        then concatMap (++"\\") $ take (n - 1) inputSplit 
        else if input =~ ".+(/|\\)$" then input else ""
  fileCompleterFromRelDir prevDir input'

fileCompleterFromRelDir :: String -> String -> IO FileCompleter
fileCompleterFromRelDir prevDir prefix = do
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
  fileCompleterFromDir dir
  
fileCompleterFromDir :: String -> IO FileCompleter
fileCompleterFromDir dir = do
  files <- listDirectory dir
  -- Hacky
  let withCd  = (dir++) <$> files
      withCd' = backslashToForward <$> withCd
  return $ FileCompleter (files ++ withCd ++ withCd')

backslashToForward = fmap (\c -> case c of
  '\\' -> '/'
  x -> x)

instance Completer Char FileCompleter where
  complete (FileCompleter fs) prefix = fromMaybe "" $ listToMaybe $ candidates
    where candidates = filter (startsWith prefix) fs

startsWith :: String -> String -> Bool
startsWith xs ys = and $ zipWith (==) xs ys
