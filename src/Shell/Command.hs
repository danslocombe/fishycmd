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

import Data.Maybe
import Data.List (nub)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Data.List.Split
import System.Cmd
import System.Console.ANSI
import System.Directory
import Data.List.Zipper hiding (insert)
import qualified Data.Map.Lazy as Map

data CommandInput = Text (Zipper Char)
                  | Complete
                  | PartialComplete
                  | Run
                  | Exit
                  | Execute String
                  | PrepControlChar
                  | HistoryBack
                  | HistoryForward

-- We have an idea of 'special' commands that hold side effects
-- these are handled by the shell rather than external calls

data SpecialCommand = CD

specialCommandMap = [("cd", CD)]

runSpecial :: [String] -> SpecialCommand -> StateT FishyState IO ()
runSpecial args cmd = do
  ifDebug $ putStrLn "Running special command..."
  case cmd of
    CD -> let arg = (case args of
                       [] -> ""
                       [""] -> ""
                       xs -> foldr1 (\x y -> x ++ " " ++ y) xs)
      in fishyCD arg

fishyCD :: String -> StateT FishyState IO ()
fishyCD "" = lift $ (putStrLn =<< getCurrentDirectory)
fishyCD arg = do 
  ifDebug $ putStrLn ("Cd ing to \"" ++ arg ++ "\"")
  exists <- lift $ doesPathExist arg
  if exists
  then do
    lift $ setCurrentDirectory arg
    dir <- lift $ getCurrentDirectory
    lift $ return ()
    -- files <- lift $ listDirectory dir
    -- state <- get
    -- fileTries' <- lift $ buildFileTries dir
    -- put state {getFileTries = fileTries'}
  else lift $ putStrLn "Error: fishy directory"

execCommand :: String -> StateT FishyState IO ()
execCommand "" = lift $ putStr "\n"
execCommand c = case splitOn " " c of 
  -- Extract first 'word'
  (x:xs) -> do
    lift $ putStr "\n"
    -- Try and match against a special command, otherwise act normal
    let special = Prelude.lookup x specialCommandMap
    case special of
      Just specialCmd -> runSpecial xs specialCmd
      Nothing -> lift $ system c >> return ()
    lift $ putStr "\n"
  -- Blank input
  _ -> lift $ return ()

data CommandProcessResult = CommandProcessResult [String] Bool Bool

processChar :: CompletionHandlerResult -> CommandInput -> StateT FishyState IO CommandProcessResult
processChar handlerResult ci = do
  -- Scrape info from state
  state <- get
  let s = toList $ getPrompt state
      historyLogs = getHistoryLogs state
      defaultReturn = return $ CommandProcessResult [] True False
      (Completion completion, _) = firstCompletionResult handlerResult

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

    -- Exit the shell
    Exit -> return $ CommandProcessResult [] False True

    -- Run what is entered by user
    Run -> do
      dirToInsert <- lift $ getCurrentDirectory
      exitcode <- execCommand s
      state' <- get
      put $ state' {getPrompt = empty}
      lift $ return $ CommandProcessResult [s] True False

    -- Should we cycle full completions?
    Complete -> do
      lift $ setCursorColumn 0
      lift clearFromCursorToLineEnd
      put state 
        { getPrompt = Zip (reverse completion) []
        , getControlPrepped = False}
      defaultReturn
      
    PartialComplete -> do
      let promptStr = toList $ getPrompt state
          compNowSplit = splitCompletion promptStr completion
          handler = getCompletionHandler state

          handler' = cycleCompletionHandler handler
          (CompletionHandlerResult comps _) = handlerResult
          comps' = nub comps
          i = getCycle handler
          compNow = fromMaybe [] 
                    ((\(Completion x) -> x) <$> (comps' !%! i))
          i' = getCycle handler'
          compNext = fromMaybe [] 
                    ((\(Completion x) -> x) <$> (comps' !%! i'))

      put $ 
        if compNow == promptStr then
          -- Cycle to next completion
          state { getCompletionHandler = handler'
                , getPrompt = Zip (reverse compNext) []
                , getControlPrepped = False }
        else
          -- Complete to partial
          state { getPrompt = Zip (reverse compNowSplit) []
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
