{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module CLI.ShellMode where

import CLI.State
import CLI.Types
import CLI.Helpers
import CLI.ShellMode.CompletionHandler
import CLI.ShellMode.Effect
import CLI.ShellMode.Draw
import CLI.SearchMode()

import Search()

import System.Console.ANSI
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.RWS.Class
import Data.List.Zipper (Zipper (..), toList)
import Data.List (nub)
import Data.Maybe (maybeToList)
import System.Console.Terminal.Size

import Complete
import Complete.String()

shellUpdate :: CommandInput -> FishyMonad (Maybe CLIMode)
shellUpdate Exit = return Nothing
shellUpdate Search = return $ Just SearchMode
shellUpdate command = do

  aliases <- getAliases <$> get
  cachedCompletionResult <- getCachedCompletions <$> get

  -- Run user input
  commandResult <- processCommand
    aliases
    cachedCompletionResult
    command
  
  modify (\s -> s { getBufferedCommands = getNewCommands commandResult
          , getCurrentDir = getCommandLocation commandResult }
          )

  whenM (return $ getRebuildCompleters commandResult) $ do
      s <- get
      ch' <- liftIO $ updateCompletionHandler 
        (getCompletionHandler s)
        (getPrompt s)
        (getCommandLocation commandResult)
        (getNewCommands commandResult)
      -- Return old handler
      let completionResult = (getCurrentCompletion ch') (toList $ getPrompt s) (getCurrentDir s)

      let s' = s {getCompletionHandler = ch', getCachedCompletions = completionResult}
      put s'
      return ch'

  -- If we have performed some non-trivial action
  -- save the state
  let newCommands = getNewCommands commandResult
  when (length newCommands > 0) $ do
      addToHistory newCommands
      saveState'

  -- log the completions
  -- s' <- get
  -- getDebug s' ?-> return ()
    --cd <- liftIO $ getCurrentDirectory
    --liftIO $ logCompletions (toList $ getPrompt s') cd completionResult

  let nextMode = if getExit commandResult
        then Nothing
        else Just ShellMode

  return nextMode

shellDraw :: FishyMonad ()
shellDraw = (getCachedCompletions <$> get) >>= drawState

drawShellMode :: Int -> (Zipper Char) -> StringCompletion -> Color -> IO Int
drawShellMode lastHeight prompt (Completion completion _) color = do
  Just (Window _ ww) <- size
  preprompt <- prePrompt
  let completionLen = length completion
  -- Show all tries
  drawCompletion lastHeight preprompt prompt completion color
  let lenTotal = length preprompt + max (length (toList prompt)) completionLen
  return $ 1 + (lenTotal `div` ww)

-- TODO refactor into io instead of fishymonad
drawState :: CompletionHandlerResult -> FishyMonad ()
drawState result = do
  state <- get
  let lastHeight = lastPromptHeight state
      prompt@(Zip _ promptR) = getPrompt state
      (completion, color) = cycledCompletionResult (getCompletionHandler state) result
      -- Warning! Hacks!
      completion' = if 
      -- If we are cycling through partial completions don't draw anything
        (getCycle . getCompletionHandler) state > 1 ||
      -- If cursor is not at end of line don't draw anything
        length promptR > 0 ||
      -- If prompt is empty then dont draw anything
        length (toList prompt) == 0

        then Completion "" 0
        else completion

  lph <- liftIO $ drawShellMode lastHeight prompt completion' color
  put $ state {lastPromptHeight = lph}

saveState' :: FishyMonad ()
saveState' = do
  state <- get
  liftIO $ saveState state

addToHistory :: [String] -> FishyMonad ()
addToHistory newCommands = do
  state <- get
  ifDebug $ putStrLn $ "\nAdding to history: " ++ show newCommands
  ifDebug $ putStrLn $ "\nCurrent history: " ++ (show $ getHistoryLogs state)
  let (Zip historyL historyR) = getHistoryLogs state
      -- hi = getHistoryIndex state
      stashCommands = maybeToList $ getHistoryStash state
      historyL' = if (all whitespace newCommands) 
        then reverse historyR ++ stashCommands ++ historyL
        else case historyL of 
          (x:_) -> (filter (/= x) newCommands) ++ (reverse historyR) ++ stashCommands ++ historyL
          _ -> nub newCommands ++ reverse historyR ++ stashCommands

  ifDebug $ putStrLn $ "\nNew commands: " ++ (show historyL') ++ " || " ++ (show historyR)
  put state
    { getHistoryLogs = Zip historyL' []
    , getHistoryStash = Nothing
    --, getHistoryIndex = foldl hiNewCommand hi newCommands
    }

whitespace :: String -> Bool
whitespace = all (== ' ')
