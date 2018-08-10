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
import Data.List.Zipper (Zipper (..), toList)

searchUpdate :: CommandInput -> FishyMonad (Maybe CLIMode)
searchUpdate ci = case ci of
  Text prompt -> do
    modify (\s ->
      s { getPrompt = prompt })
    return $ Just SearchMode

  Run -> do
    return $ Just ShellMode

  _ -> return $ Just ShellMode

searchDraw :: FishyMonad()
searchDraw = do
  pp <- liftIO prePrompt
  state <- get
  let s = toList (getPrompt state)

  liftIO $ setCursorColumn 0
  liftIO $ clearFromCursorToLineEnd
  liftIO $ putStrLn "Search Results:"
  liftIO $ cursorDown 2

  let lookups = hiLookupCommand (getHistoryIndex state) s :: [(String, [String], Int)]
  liftIO $ mapM (\xs@(x, _, _) -> do
    setCursorColumn 0
    clearFromCursorToLineEnd
    putStrLn $ show xs
    ) lookups

  liftIO $ cursorUp (length lookups)

  --liftIO $ cursorDown 1
  --liftIO $ cursorUp 5

  liftIO $ setCursorColumn 0
  liftIO clearFromCursorToLineEnd
  liftIO $ putStr pp
  liftIO $ putStr s
  liftIO $ hFlush stdout

  return ()
  
  
