module Shell.Update (updateIOState) where

import Complete
import Complete.String
import Complete.FileCompleter
import Complete.Completer
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

draw :: Int -> (Zipper Char) -> StringCompletion -> Color -> IO Int
draw lastHeight prompt completion color = do
  Just (Window _ ww) <- size
  preprompt <- prePrompt
  cd <- getCurrentDirectory
  --let rs = fishyComplete state cd
      --prompt = getPrompt state
      --(completion, color) = toDraw rs (toList prompt)
  let completionLen = length ((\(Completion c) -> c) completion)
  -- Show all tries
  --putStrLn $ show $ (\(FishyCompleterResult x _) -> x) <$> rs
  drawCompletion lastHeight preprompt prompt completion color
  let lenTotal = length preprompt + max (length (toList prompt)) completionLen
  return $ 1 + (lenTotal `div` ww)

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

getCurrentCompletionWrapper :: StateT FishyState IO CompletionHandlerResult
getCurrentCompletionWrapper = do
  state <- get
  dir <- lift $ getCurrentDirectory
  let handler = getCompletionHandler state
      prompt = getPrompt state
      prefix = toList prompt
      -- For now all targets but limit based on current prefix
      def = CompletionHandlerResult [Completion []] Red
  -- return $ fromMaybe def $ getCurrentCompletion handler prefix dir targets
  return $ getCurrentCompletion handler prefix dir
  
processCharWrap :: CompletionHandlerResult -> Char -> StateT FishyState IO CommandProcessResult
processCharWrap completerResult c = do
  state <- get
  currentDir <- lift $ getCurrentDirectory
  let ci = (matchChar state currentDir) c
  processChar completerResult ci

updateIOState :: CommandProcessResult -> StateT FishyState IO CommandProcessResult
updateIOState (CommandProcessResult commands doUpdate _) = do
  -- TODO use guard
  completion <- if doUpdate
    then do 
      updateCompletionHandlerWrap commands
      cs <- getCurrentCompletionWrapper
      state <- get
      put $ state {getCachedCompletions = cs}
      lift $ return cs
    else do
      state <- get
      --lift $ putStrLn ("Getting cached" ++ (show (getCachedCompletions state)))
      lift $ return $ getCachedCompletions state
  -- Draw completion then yield for next char
  drawStateWrap completion
  c <- lift getHiddenChar
  processCharWrap completion c
