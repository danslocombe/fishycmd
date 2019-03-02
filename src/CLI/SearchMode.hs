{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module CLI.SearchMode where


import CLI.State
import CLI.Types

import CLI.ShellMode.Draw (prePrompt)

import Search

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.RWS.Class
import System.Console.ANSI
import System.IO
import Data.List.Zipper (Zipper (..), toList, fromList)

searchUpdate :: CommandInput -> FishyMonad (Maybe CLIMode)
searchUpdate ci = case ci of
  Text prompt -> do
    modify (\s ->
      s { getPrompt = prompt })
    return $ Just SearchMode

  Run -> do
    s <- get
    let lookups = execSearch s
    case lookups of
      [] -> return ()
      (x:xs) -> modify (\s -> s {getPrompt = Zip (reverse x) []})

    return $ Just ShellMode

  _ -> return $ Just ShellMode

execSearch :: FishyState -> [String]
execSearch state = res
  where 
    query = toList $ getPrompt state 
    hi = getHistoryIndex state
    allLookups = hiLookupCommand hi query :: [(String, [String], Int)]
    lookups = filter (\(_,_,score) -> score > 1) $ allLookups

    --res = show <$> lookups
    res = (\(x,_,_) -> x) <$> lookups


maxResults = 5

searchDraw :: FishyMonad()
searchDraw = do
  pp <- liftIO prePrompt
  state <- get
  let lookups = take maxResults $ execSearch state

  -- TODO rember current search session max height
  liftIO $ replicateM_ (1 + maxResults) $ do -- (1 + length lookups) $ do
    cursorUp 1
    setCursorColumn 0
    clearFromCursorToLineEnd

  if length lookups == 0
    then liftIO $ putStrLn "Searching: "
    else liftIO $ putStrLn "Search Results:"

  liftIO $ mapM (\(i, x) -> do
    -- Debug show all
    putStrLn $ "(" ++ show i ++ ") " ++ show x
    ) $ zip [0..] lookups

  --liftIO $ cursorUp (length lookups)

  --liftIO $ cursorDown 1
  --liftIO $ cursorUp 5

  liftIO $ cursorDown (maxResults - length lookups)

  liftIO $ setCursorColumn 0
  liftIO clearFromCursorToLineEnd
  liftIO $ putStr "> "
  --liftIO $ putStr pp
  liftIO $ putStr $ toList $ getPrompt state
  liftIO $ hFlush stdout

  return ()
  
  
