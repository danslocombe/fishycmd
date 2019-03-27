{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module CLI.SearchMode where


import CLI.State
import CLI.Types
import CLI.Helpers

import CLI.ShellMode.Draw (prePrompt)

import Search

import System.IO
import System.Console.ANSI
import System.Console.ANSI
import System.Console.Terminal.Size

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.RWS.Class

import Data.List.Zipper (Zipper (..), toList, fromList)
import Data.Maybe (fromMaybe)

searchUpdate :: CommandInput -> FishyMonad (Maybe CLIMode)
searchUpdate ci = case ci of
  Text prompt -> do
    modify (\s ->
      s { getPrompt = prompt })
    return $ Just SearchMode

  PartialComplete -> selectEndSearch
  Complete        -> selectEndSearch
  Run             -> selectEndSearch

  _ -> return $ Just ShellMode

selectEndSearch :: FishyMonad (Maybe CLIMode)
selectEndSearch = do
  setPromptTopResult
  -- Clear temp index
  modify (\s -> s {getHistoryIndex = Nothing})
  return $ Just ShellMode

setPromptTopResult :: FishyMonad ()
setPromptTopResult = do
  hi <- getIndex
  state <- get
  let query = toList $ getPrompt state 
  let lookups = execSearch hi query
  case lookups of
    [] -> return ()
    (x:xs) -> modify (\s -> s {getPrompt = Zip (reverse x) []})

getIndex :: FishyMonad HistoryIndex
getIndex = do 
    s <- get
    case getHistoryIndex s of
      Just hi -> return hi
      Nothing -> do
        let hi' = hiFromCommands $ toList $ getHistoryLogs s
        modify (\s' -> s' {getHistoryIndex = Just hi'})
        return hi'

execSearch :: HistoryIndex -> String -> [String]
execSearch hi query  = res
  where 
    allLookups = hiLookupCommand hi query :: [(String, [String], Int)]
    lookups = filter (\(_,_,score) -> score > 1) $ allLookups

    --res = show <$> lookups
    res = (\(x,_,_) -> x) <$> lookups


maxResults = 5

searchDraw :: FishyMonad ()
searchDraw = do
  pp <- liftIO prePrompt
  state <- get
  hi <- getIndex 
  let query = toList $ getPrompt state 
  let lookups = take maxResults $ execSearch hi query 

  -- TODO rember current search session max height
  liftIO $ replicateM_ (1 + maxResults) $ do -- (1 + length lookups) $ do
    cursorUp 1
    setCursorColumn 0
    clearFromCursorToLineEnd

  liftIO $ putStrLn "Fishy Search: "

  liftIO $ mapM (\(i, x) -> do
    let s = "(" ++ show i ++ ") " ++ show x

    if i == 0 
      then do
        setSGR [SetColor Foreground Vivid Red]
        putStrLn s
        setSGR [Reset]
      else
        putStrLn s 
    ) $ zip [0..] lookups

  --liftIO $ cursorUp (length lookups)

  --liftIO $ cursorDown 1
  --liftIO $ cursorUp 5

  liftIO $ cursorDown (maxResults - length lookups)

  liftIO $ setCursorColumn 0
  liftIO clearFromCursorToLineEnd
  liftIO $ putStr "> "
  --liftIO $ putStr pp
  liftIO $ putStr query
  liftIO $ hFlush stdout

  return ()
  
  
