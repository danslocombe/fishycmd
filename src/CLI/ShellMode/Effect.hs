{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module CLI.ShellMode.Effect where

import Complete
import Complete.String()

import CLI.State
import CLI.Types
import CLI.Helpers
import CLI.ShellMode.CompletionHandler
import CLI.ShellMode.FishyCommand

import System.Signal
import Data.Maybe
import Data.List (nub)
import Data.Char (isSpace)
import Data.List.Split
import System.Process
import System.Console.ANSI
import System.Directory
import Control.Monad.IO.Class
import Control.Monad.RWS.Class
import System.IO
import Data.List.Zipper hiding (insert)

import Corext.AliasCompleter

killHandler :: ProcessHandle -> Handler
killHandler phandle _ = interruptProcessGroupOf phandle

execCommand :: String -> FishyMonad Bool
execCommand "" = liftIO $ putStr "\n" >> return False
execCommand c = case splitOn " " c of 
  -- Extract first 'word'
  (commandWord:commandWords) -> do
    liftIO $ putStr "\n"
    -- Try and match against a fishy command, otherwise act normal
    let fishy = Prelude.lookup commandWord fishyCommandMap
    ret <- case fishy of
      -- Run fishycommand
      Just fishyCmd -> runFishy commandWords fishyCmd
      Nothing -> do 

        -- Setup flags for process
        let x = (shell c) {create_group = True}

        -- Spawn Process
        (_, _, _, phandle) <- liftIO $ createProcess x

        -- Should we block for the result?
        whenM (liftIO $ blockForCommand c) $ do
          -- Install an interrupt handler that kills the child process
          liftIO $ installHandler sigINT 
            (\sig -> do {
                killHandler phandle sig; hFlush stdout})
          liftIO $ waitForProcess phandle

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
        , getHistoryStash = Nothing
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
                , getHistoryStash = Nothing
                , getControlPrepped = False }
        else
          -- Complete to partial
          state { getPrompt = Zip (reverse compNowSplit) promptR
                , getHistoryStash = Nothing
                , getControlPrepped = False }
      liftIO $ setCursorColumn 0
      liftIO clearFromCursorToLineEnd
      liftIO $ return $ CommandProcessResult [] initialLocation False False

    -- Execute some other command
    Execute command -> do
      _exitcode <- liftIO $ system command
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

    Search -> error "ShellMode does not handle search"
      

-- TODO factor out some of following common in both functions
backHistory :: FishyMonad ()
backHistory = changeHistory pushTopZipper popBotZipper

forwardHistory :: FishyMonad ()
forwardHistory = changeHistory pushBotZipper popTopZipper

type PushNewHistory = String -> Zipper String -> Zipper String
type PopHistory = Zipper String -> (Zipper String, Maybe String)
changeHistory :: PushNewHistory -> PopHistory -> FishyMonad ()
changeHistory modifyHistory getFromHistory = do
    state <- get
    let stash = maybe "" id $ getHistoryStash state
        history = getHistoryLogs state
        (history', mText) = getFromHistory history
        (text, history'') = case mText of
          Just newText -> (newText, case stash of
            "" -> history'
            _  -> modifyHistory stash history')
          Nothing      -> (stash, history')
        prompt = Zip (reverse text) []
    put state { getHistoryLogs = history'', getPrompt = prompt, getHistoryStash = Just text }

-- This is incredibly hacky
-- We have a whitelist of file extensions where running will execute in the background
-- Main use for this is running "Example.csproj" to open in visual studio
blockForCommand :: String -> IO Bool
blockForCommand c = do
  exists <- doesFileExist c
  let hasAsyncExt = (maybe False (`elem` asyncExtensions) (extension c))
  return $ not $ exists && hasAsyncExt

asyncExtensions :: [String]
asyncExtensions = ["csproj", "sln", "txt", "cs", "bond", "ini", "log"]

extension :: String -> Maybe String
extension s = case dropWhile (/= '.') s of
  (_:ext) -> Just $ maybe ext id (extension ext)
  _ -> Nothing
