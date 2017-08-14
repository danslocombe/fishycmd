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
drawStateWrap :: CompletionHandlerResult -> StateT FishyState IO ()
drawStateWrap result = do
  state <- get
  let lastHeight = lastPromptHeight state
      prompt = getPrompt state
      (completion, color) = firstCompletionResult result
      -- Warning! Hack!
      completion' = if (getCycle . getCompletionHandler) state > 1
        then Completion ""
        else completion

  lph <- lift $ draw lastHeight prompt completion' color
  put $ state {lastPromptHeight = lph}

-- Wrap updating completion handler with updates to state
updateCompletionHandlerWrap :: [String] -> StateT FishyState IO ()
updateCompletionHandlerWrap newCommands = do
  state <- get
  ch <- lift $ updateCompletionHandler 
    (getCompletionHandler state) 
    (getPrompt state)
    <$> getCurrentDirectory
    <*> return newCommands
    
  state' <- lift $ fmap (\x -> state {getCompletionHandler = x}) ch
  put $ state' {getBufferedCommands = []}

-- Package getting the current state and executing the completion handler
getCurrentCompletionWrapper :: StateT FishyState IO CompletionHandlerResult
getCurrentCompletionWrapper = do
  state <- get
  dir <- lift $ getCurrentDirectory
  let handler = getCompletionHandler state
      prompt = getPrompt state
      prefix = toList prompt
      def = CompletionHandlerResult [Completion []] Red
  return $ getCurrentCompletion handler prefix dir
  
-- Wrap processing an input char with updating state
processCharWrap :: CompletionHandlerResult -> Char -> StateT FishyState IO CommandProcessResult
processCharWrap completerResult c = do
  state <- get
  currentDir <- lift $ getCurrentDirectory
  let ci = (matchChar state currentDir) c
  processChar completerResult ci

-- Main loop
updateIOState :: CommandProcessResult -> StateT FishyState IO CommandProcessResult
updateIOState (CommandProcessResult commands doUpdate _) = do
  -- TODO use guard, move to own function
  completion <- if doUpdate
    then do 
      updateCompletionHandlerWrap commands
      cs <- getCurrentCompletionWrapper
      state <- get
      put $ state {getCachedCompletions = cs}
      lift $ return cs
    else do
      state <- get
      lift $ return $ getCachedCompletions state
  -- Draw completion then yield for next char
  drawStateWrap completion
  c <- lift getHiddenChar
  processCharWrap completion c
