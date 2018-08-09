{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module CLI.ShellMode.Effect where

import Complete.String
import Complete
import CLI.State
import CLI.Types
import CLI.Helpers
import CLI.ShellMode.CompletionHandler
import CLI.ShellMode.FishyCommand
import CLI.ShellMode.Prompt

import System.Signal
import Data.Maybe
import Data.List (nub, lookup)
import Data.Char (isSpace)
import Data.List.Split
import System.Process
import System.Console.ANSI
import System.Directory
import Control.Monad.IO.Class
import Control.Monad.RWS.Class
import System.IO
import Data.List.Zipper hiding (insert)
import qualified Data.Map.Lazy as Map

import Corext.AliasCompleter

killHandler :: ProcessHandle -> Handler
killHandler phandle _ = interruptProcessGroupOf phandle

execCommand :: String -> FishyMonad Bool
execCommand "" = liftIO $ putStr "\n" >> return False
execCommand c = case splitOn " " c of 
  -- Extract first 'word'
  (x:xs) -> do
    liftIO $ putStr "\n"
    -- Try and match against a fishy command, otherwise act normal
    let fishy = Prelude.lookup x fishyCommandMap
    ret <- case fishy of
      -- Run fishycommand
      Just fishyCmd -> runFishy xs fishyCmd
      Nothing -> do 

        -- Setup flags for process
        let x = (shell c) {create_group = True}

        -- Spawn Process
        (_, _, _, phandle) <- liftIO $ createProcess x

        -- Should we block for the result?
        (liftIO $ blockForCommand c) ?~> do
          --liftIO $ putStrLn "blocking"
          -- Install an interrupt handler that kills the child process
          liftIO $ installHandler sigINT 
            (\sig -> do {
                killHandler phandle sig; hFlush stdout})
          liftIO $ waitForProcess phandle
          --liftIO $ putStrLn "done"

        -- Update state
        modify (\s ->
                 s { getPrompt = empty
                   , getCachedCompletions = CompletionHandlerResult [] Red}
               )

        return False

    liftIO $ putStr "\n"
    return ret

  -- Blank input
  _ -> return False

processCommand :: [Alias] -> CompletionHandlerResult -> CommandInput -> FishyMonad CommandProcessResult
processCommand aliases handlerResult ci = do
  -- Scrape info from state
  state <- get
  initialLocation <- liftIO $ getCurrentDirectory
  let s = toList $ getPrompt state
      (Zip promptL promptR) = getPrompt state
      historyLogs = getHistoryLogs state
      defaultReturn = return $ CommandProcessResult [] initialLocation True False
      (Completion completion _, _) = firstCompletionResult handlerResult

  case ci of 
    -- Update using new prompt state
    Text prompt -> do
      liftIO $ setCursorColumn 0
      liftIO clearFromCursorToLineEnd
      put state 
        { getPrompt = prompt
        , getCompletionHandler = resetCompletionHandler (getCompletionHandler state)
        , getControlPrepped = False}
      defaultReturn

    Cls -> do
      execCommand "cls"
      return $ CommandProcessResult [] initialLocation False False

    -- Exit the shell
    Exit -> return $ CommandProcessResult [] initialLocation False True

    -- Run what is entered by user
    Run -> do
      dirToInsert <- liftIO $ getCurrentDirectory
      let trim :: String -> String
          trim = f . f
            where f = reverse . dropWhile isSpace
          trimmed = trim s
          toExec = aliasComplete aliases $ trimmed
      exitQuestionMark <- execCommand toExec
      state' <- get
      put $ state' {getPrompt = empty}
      return $ CommandProcessResult [trimmed] initialLocation True exitQuestionMark

    -- Should we cycle full completions?
    Complete -> do
      liftIO $ setCursorColumn 0
      liftIO clearFromCursorToLineEnd
      put state 
        { getPrompt = Zip (reverse completion) []
        , getControlPrepped = False}
      defaultReturn
      
    PartialComplete -> do
      let prefix = reverse promptL
          compNowSplit = splitCompletion prefix completion
          handler = getCompletionHandler state

          handler' = cycleCompletionHandler handler
          (CompletionHandlerResult comps _) = handlerResult
          comps' = nub comps
          i = getCycle handler
          compNow = fromMaybe [] 
                    ((\(Completion x _) -> x) <$> (comps' !%! i))
          i' = getCycle handler'
          compNext = fromMaybe [] 
                    ((\(Completion x _) -> x) <$> (comps' !%! i'))

      put $ 
        if compNow == prefix then
          -- Cycle to next completion
          state { getCompletionHandler = handler'
                , getPrompt = Zip (reverse compNext) promptR
                , getControlPrepped = False }
        else
          -- Complete to partial
          state { getPrompt = Zip (reverse compNowSplit) promptR
                , getControlPrepped = False }
      liftIO $ setCursorColumn 0
      liftIO clearFromCursorToLineEnd
      liftIO $ return $ CommandProcessResult [] initialLocation False False

    -- Execute some other command
    Execute command -> do
      exitcode <- liftIO $ system command
      defaultReturn

    -- Some inputs (up, down, etc) are represented by two characters
    -- The 'control character' then some other, this is called on input of the control char
    PrepControlChar -> do
      put state {getControlPrepped = True}
      defaultReturn

    -- Move back through history
    HistoryBack -> do
      backHistory
      state' <- get
      ifDebug $ putStrLn ("\nBack History: " ++ show (getHistoryLogs state'))
      defaultReturn

    -- Move forwards through history
    HistoryForward -> do
      forwardHistory
      state' <- get
      ifDebug $ putStrLn ("\nForwards History: " ++ show (getHistoryLogs state'))
      defaultReturn

-- TODO factor out some of following common in both functions
backHistory :: FishyMonad ()
backHistory = do
    state <- get
    let s = toList $ getPrompt state
        history = getHistoryLogs state
        (history', mText) = popBotZipper history
        (text, history'') = case mText of
          Just newText -> (newText, case s of
            "" -> history'
            _  -> pushTopZipper s history')
          Nothing      -> (s, history')
        prompt = Zip (reverse text) []
    put state { getHistoryLogs = history'', getPrompt = prompt }

forwardHistory :: FishyMonad ()
forwardHistory = do
    state <- get
    let s = toList $ getPrompt state
        history = getHistoryLogs state
        (history', mText) = popTopZipper history
        (text, history'') = case mText of
          Just newText -> (newText, case s of
            "" -> history'
            _  -> pushBotZipper s history')
          Nothing      -> (s, history')
        prompt = Zip (reverse text) []
    put state { getHistoryLogs = history'', getPrompt = prompt }


-- This is incredibly hacky
blockForCommand :: String -> IO Bool
blockForCommand c = do
  exists <- doesFileExist c
  let hasAsyncExt = (maybe False (`elem` asyncExtensions) (extension c))
  return $ not $ exists && hasAsyncExt

asyncExtensions = ["csproj", "sln", "txt", "cs", "bond", "ini", "log"]

extension :: String -> Maybe String
extension s = case dropWhile (/= '.') s of
  (_:ext) -> Just ext
  _ -> Nothing
