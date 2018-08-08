{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Shell.ShellMode where

import Shell.Types
import Shell.ShellMode.CompletionHandler
import Shell.ShellMode.Effect
import Shell.ShellMode.Draw

import System.Console.ANSI
import Control.Monad.IO.Class
import Control.Monad.RWS.Class
import Data.List.Zipper (Zipper (..), toList)
import System.Console.Terminal.Size
import System.Directory

-- todo these shouldnt be dependencies
import Complete
import Complete.String

shellMode :: Mode
shellMode = Mode
  { update = smUpdate
  , draw = smDraw
  }

smUpdate :: CommandInput -> FishyMonad (Maybe Mode)
smUpdate Exit = return Nothing
smUpdate command = do

  aliases <- getAliases <$> get
  cachedCompletionResult <- getCachedCompletions <$> get

  -- Run user input
  commandResult <- processCommand
    aliases
    cachedCompletionResult
    command
  
  let rebuildCompleters = getRebuildCompleters commandResult
      nextMode = if getExit commandResult
        then Nothing
        else Just shellMode

  modify (\s -> s { getBufferedCommands = getNewCommands commandResult
          , getCurrentDir = getCommandLocation commandResult }
          )

  ch' <- if getRebuildCompleters commandResult 
      -- Update completion handler
    then do
      s <- get
      ch' <- liftIO $ updateCompletionHandler 
        (getCompletionHandler s)
        (getPrompt s)
        (getCurrentDir s)
        (getNewCommands commandResult)
      -- Return old handler
      put $ s {getCompletionHandler = ch'}
      return ch'
    else (getCompletionHandler <$> get)

  -- Update search index

  -- Run completion handler
  s <- get
  let completionResult = (getCurrentCompletion ch') (toList $ getPrompt s) (getCurrentDir s)

  put $ s {getCachedCompletions = completionResult}

  return nextMode

smDraw :: FishyMonad ()
smDraw = (getCachedCompletions <$> get) >>= drawState
  --drawCompletion lastPromptHeight pp getPrompt currentCompletion Red

drawW :: Int -> (Zipper Char) -> StringCompletion -> Color -> IO Int
drawW lastHeight prompt (Completion completion _) color = do
  Just (Window _ ww) <- size
  preprompt <- prePrompt
  cd <- getCurrentDirectory
  let completionLen = length completion
  -- Show all tries
  --putStrLn $ show $ (\(FishyCompleterResult x _) -> x) <$> rs
  drawCompletion lastHeight preprompt prompt completion color
  let lenTotal = length preprompt + max (length (toList prompt)) completionLen
  return $ 1 + (lenTotal `div` ww)

drawState :: CompletionHandlerResult -> FishyMonad ()
drawState result = do
  state <- get
  let lastHeight = lastPromptHeight state
      prompt@(Zip promptL promptR) = getPrompt state
      (completion, color) = firstCompletionResult result
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

  lph <- liftIO $ drawW lastHeight prompt completion' color
  put $ state {lastPromptHeight = lph}

saveState' :: FishyMonad ()
saveState' = do
  state <- get
  liftIO $ saveState state

addToHistory :: [String] -> FishyMonad ()
addToHistory cs = do
  state <- get
  let (Zip historyL historyR) = getHistoryLogs state
  put state {getHistoryLogs = Zip (cs ++ historyL) historyR}

