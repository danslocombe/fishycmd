module Shell.Command 
    --( processKeyPress 
    ( processChar
    , CommandProcessResult (..)
    , CommandInput         (..)
    ) where

import Complete.String
import Complete
import Shell.State
import Shell.CompleteHandler
import Shell.Types
import Shell.FishyCommand

import System.Signal
import Data.Maybe
import Data.List (nub, lookup)
import Data.Char (isSpace)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Data.List.Split
import System.Process
import System.Console.ANSI
import System.Directory
import System.IO
import Data.List.Zipper hiding (insert)
import qualified Data.Map.Lazy as Map

import Corext.AliasCompleter

data CommandInput = Text (Zipper Char)
                  | Cls
                  | Complete
                  | PartialComplete
                  | Run
                  | Exit
                  | Execute String
                  | PrepControlChar
                  | HistoryBack
                  | HistoryForward

killHandler :: ProcessHandle -> Handler
killHandler phandle _ = interruptProcessGroupOf phandle

execCommand :: String -> StateT FishyState IO Bool
execCommand "" = lift $ putStr "\n" >> return False
execCommand c = case splitOn " " c of 
  -- Extract first 'word'
  (x:xs) -> do
    lift $ putStr "\n"
    -- Try and match against a fishy command, otherwise act normal
    let fishy = Prelude.lookup x fishyCommandMap
    ret <- case fishy of
      Just fishyCmd -> runFishy xs fishyCmd
      Nothing -> do 
        let x = (shell c) {create_group = True}
        (_, _, _, phandle) <- lift $ createProcess x
        lift $ installHandler sigINT 
          (\sig -> do {
              -- putStrLn "KILLING SPAWNED PROCESS"; 
              killHandler phandle sig; hFlush stdout})
        lift $ waitForProcess phandle
        s <- get
        put $ s {getPrompt = empty, getCachedCompletions = CompletionHandlerResult [] Red}
        s' <- get
        lift $ return False
    lift $ putStr "\n"
    lift $ return ret
  -- Blank input
  _ -> lift $ return False

data CommandProcessResult = CommandProcessResult 
  { getNewCommands     :: [String]
  , getRebuildCompleters :: Bool 
  , getExit              :: Bool
  } deriving Show

processChar :: [Alias] -> CompletionHandlerResult -> CommandInput -> StateT FishyState IO CommandProcessResult
processChar aliases handlerResult ci = do
  -- Scrape info from state
  state <- get
  let s = toList $ getPrompt state
      (Zip promptL promptR) = getPrompt state
      historyLogs = getHistoryLogs state
      defaultReturn = return $ CommandProcessResult [] True False
      (Completion completion _, _) = firstCompletionResult handlerResult

  case ci of 
    -- Update using new prompt state
    Text prompt -> do
      lift $ setCursorColumn 0
      lift clearFromCursorToLineEnd
      put state 
        { getPrompt = prompt
        , getCompletionHandler = resetCompletionHandler (getCompletionHandler state)
        , getControlPrepped = False}
      defaultReturn

    Cls -> do
      execCommand "cls"
      return $ CommandProcessResult [] False False

    -- Exit the shell
    Exit -> return $ CommandProcessResult [] False True

    -- Run what is entered by user
    Run -> do
      dirToInsert <- lift $ getCurrentDirectory
      let trim :: String -> String
          trim = f . f
            where f = reverse . dropWhile isSpace
          toExec = aliasComplete aliases $ trim s
      exitQuestionMark <- execCommand toExec
      state' <- get
      put $ state' {getPrompt = empty}
      lift $ return $ CommandProcessResult [toExec] True exitQuestionMark

    -- Should we cycle full completions?
    Complete -> do
      lift $ setCursorColumn 0
      lift clearFromCursorToLineEnd
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
      lift $ setCursorColumn 0
      lift clearFromCursorToLineEnd
      lift $ return $ CommandProcessResult [] False False

    -- Execute some other command
    Execute command -> do
      exitcode <- lift $ system command
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
backHistory :: StateT FishyState IO ()
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

forwardHistory :: StateT FishyState IO ()
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


-- Functions for treating zipper as two stacks

pushTopZipper :: a -> Zipper a -> Zipper a
pushTopZipper y (Zip xs ys) = Zip xs (y : ys)

pushBotZipper :: a -> Zipper a -> Zipper a
pushBotZipper x (Zip xs ys) = Zip (x : xs) ys

popTopZipper :: Zipper a -> (Zipper a, Maybe a)
popTopZipper (Zip xs (y:ys)) = (Zip xs ys, Just y)
popTopZipper (Zip xs []) = (Zip xs [], Nothing)

popBotZipper :: Zipper a -> (Zipper a, Maybe a)
popBotZipper (Zip (x:xs) ys) = (Zip xs ys, Just x)
popBotZipper (Zip [] ys) = (Zip [] ys, Nothing)

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:xs) = Just xs
