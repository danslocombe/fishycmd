module Shell.Update (updateIOState) where

import Complete
import Complete.String
import Shell.CompleteHandler
import Shell.Draw
import Shell.State
import Shell.Types
import Shell.Command
import Shell.KeyPress
import Shell.CompleteHandler
import Shell.Helpers

import Data.Char (ord)
import Data.Maybe
import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Data.List.Zipper hiding (insert)
import System.Console.Terminal.Size
import System.Console.ANSI
import System.Directory

-- Draw prompt and completion
-- Has some clumsy logic to deal with line heights
-- TODO fix
draw :: Int -> (Zipper Char) -> StringCompletion -> Color -> IO Int
draw lastHeight prompt completion color = do
  Just (Window _ ww) <- size
  preprompt <- prePrompt
  cd <- getCurrentDirectory
  let completionLen = length ((\(Completion c) -> c) completion)
  -- Show all tries
  --putStrLn $ show $ (\(FishyCompleterResult x _) -> x) <$> rs
  drawCompletion lastHeight preprompt prompt completion color
  let lenTotal = length preprompt + max (length (toList prompt)) completionLen
  return $ 1 + (lenTotal `div` ww)

-- Wrap draw function with updating of state
drawState' :: CompletionHandlerResult -> StateT FishyState IO ()
drawState' result = do
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

        then Completion ""
        else completion

  lph <- lift $ draw lastHeight prompt completion' color
  put $ state {lastPromptHeight = lph}

-- Wrap updating completion handler with updates to state
updateCompletionHandler' :: [String] -> StateT FishyState IO ()
updateCompletionHandler' newCommands = do
  state <- get
  ch <- lift $ updateCompletionHandler 
    (getCompletionHandler state) 
    (getPrompt state)
    <$> getCurrentDirectory
    <*> return newCommands
    
  state' <- lift $ fmap (\x -> state {getCompletionHandler = x}) ch
  put $ state' {getBufferedCommands = []}

-- Package getting the current state and executing the completion handler
getCurrentCompletion' :: StateT FishyState IO CompletionHandlerResult
getCurrentCompletion' = do
  state <- get
  dir <- lift $ getCurrentDirectory
  let handler = getCompletionHandler state
      prompt@(Zip promptL promptR) = getPrompt state
      def = CompletionHandlerResult [Completion []] Red
  return $ getCurrentCompletion handler (reverse promptL) dir
  
-- Wrap processing an input char with updating state
processChar' :: CompletionHandlerResult -> Char -> StateT FishyState IO CommandProcessResult
processChar' completerResult c = do
  state <- get
  currentDir <- lift $ getCurrentDirectory
  let ci = (matchChar state currentDir) c
  processChar completerResult ci

addToHistory :: [String] -> StateT FishyState IO ()
addToHistory cs = do
  state <- get
  let (Zip historyL historyR) = getHistoryLogs state
  put state {getHistoryLogs = Zip (cs ++ historyL) historyR}

saveState' :: StateT FishyState IO ()
saveState' = do
  state <- get
  lift $ saveState state

-- Main loop
updateIOState :: CommandProcessResult -> StateT FishyState IO CommandProcessResult
updateIOState (CommandProcessResult commands doUpdate _) = do
  -- Add commands to history
  addToHistory commands

  -- Check if we should update the completion handlers
  -- if we should then update and write to cache, otherwise
  -- read from cache
  -- return current completion
  completion <- if doUpdate
    then do 
      -- feed handlers new commands
      updateCompletionHandler' commands
      cs <- getCurrentCompletion'
      state <- get
      -- write to cache
      let state' = state {getCachedCompletions = cs}
      put state'
      lift $ return cs
    else do
      state <- get
      lift $ return $ getCachedCompletions state

  -- If we have performed some non-trivial action
  -- save the state
  length commands > 0
    ?-> saveState'

  -- log the completions
  ss <- get
  cd <- lift $ getCurrentDirectory
  lift $ logCompletions (toList $ getPrompt ss) cd completion

  -- Draw completion then yield for next char
  drawState' completion
  c <- lift getHiddenChar
  -- lift . putStrLn . show $ ord c
  processChar' completion c
