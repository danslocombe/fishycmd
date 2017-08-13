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
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Data.List.Zipper hiding (insert)
import System.Console.Terminal.Size
import System.Console.ANSI
import System.Directory

draw :: Int -> (Zipper Char) -> CompletionHandlerResult -> IO Int
draw lastHeight prompt (CompletionHandlerResult completion color) = do
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
  lph <- lift $ draw lastHeight prompt result
  put $ state {lastPromptHeight = lph}

updateCompletionHandlerWrap :: StateT FishyState IO ()
updateCompletionHandlerWrap = do
  state <- get
  let newCommands = getBufferedCommands state
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
      targets = 
        [ NameLocalHistoryCompleter
        , NameFileCompleter
        , NameGlobalHistoryCompleter
        , NamePathCompleter ]
      def = CompletionHandlerResult (Completion []) Red
  return $ fromMaybe def $ getCurrentCompletion handler prefix dir targets
  
processCharWrap :: CompletionHandlerResult -> Char -> StateT FishyState IO Bool
processCharWrap completerResult c = do
  state <- get
  currentDir <- lift $ getCurrentDirectory
  let ci = (matchChar state currentDir) c
  (CommandProcessResult buffered exit) <- processChar completerResult ci
  state' <- get
  put state' {getBufferedCommands = buffered}
  lift $ return exit

updateIOState :: StateT FishyState IO Bool
updateIOState = do
  updateCompletionHandlerWrap
  completion <- getCurrentCompletionWrapper
  -- Draw completion then yield for next char
  drawStateWrap completion
  c <- lift getHiddenChar
  processCharWrap completion c
