{-# LANGUAGE ForeignFunctionInterface #-}

module Update where

import Trie
import TrieState
import Draw

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Data.Char (chr, ord)
import Data.List.Split
import Data.List.Zipper
import System.Console.ANSI
import Foreign.C.Types
import System.Cmd
import GHC.IO.Exception
import System.Directory

data CommandInput = Text (Zipper Char)
                  | Run
                  | Exit
                  | Execute String
                  | PrepControlChar
                  | HistoryBack
                  | HistoryForward

getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt

-- TODO refactor this (too large)
updateIOState :: StateT CompleteState IO Bool
updateIOState = do
  state <- get
  -- lift $ putStrLn ("UpdateState: " ++ show (getHistoryLogs state))
  let p = getPrompt state
      historyLogs = getHistoryLogs state
      s = toList p
  lift $ drawCompletion state
  c <- lift getHiddenChar
  -- ifDebug (putStrLn $ show $ ord c)
  case matchChar state c of 
    Text prompt -> do
      lift $ setCursorColumn 0
      lift clearFromCursorToLineEnd
      put state {getPrompt = prompt, getControlPrepped = False}
      return False
    Exit -> return True
    Run -> do
      lift $ putStr "\n"
      exitcode <- execCommand s
      lift $ putStr "\n"
      let historyTries = insertCW s $ getHistoryTries state
      let state' = state { getHistoryTries = historyTries
        , getPrompt = empty
        , getHistoryLogs = push s historyLogs }
      put state'
      lift $ saveState state
      return False
    Execute command -> do
      exitcode <- lift $ system command
      let state' = state { getHistoryLogs = push s historyLogs }
      put state'
      lift $ return False
    PrepControlChar -> do
      put state {getControlPrepped = True}
      lift $ return False
    HistoryBack -> do
      backHistory
      state' <- get
      ifDebug $ putStrLn ("\nBack History: " ++ show (getHistoryLogs state'))
      lift $ return False
    HistoryForward -> do
      forwardHistory
      state' <- get
      ifDebug $ putStrLn ("\nForwards History: " ++ show (getHistoryLogs state'))
      lift $ return False

data SpecialCommand = CD

specialCommandMap = [("cd", CD)]

runSpecial :: [String] -> SpecialCommand -> StateT CompleteState IO ()
runSpecial args cmd = do
  ifDebug $ putStrLn "Running special command..."
  case cmd of
    CD -> case args of
      -- [dir] -> fishyCD dir >> lift (return ())
      [dir] -> fishyCD dir
      _ -> lift $ putStrLn "Error: fishy input"

-- fishyCD :: String -> StateT CompleteState IO ExitCode
fishyCD :: String -> StateT CompleteState IO ()
fishyCD arg = lift $ do 
  exists <- doesPathExist arg
  if exists
  then setCurrentDirectory arg
  else putStrLn "Error: fishy directory"

execCommand :: String -> StateT CompleteState IO ()
execCommand c = case splitOn " " c of 
  (x:xs) -> do
    let special = lookup x specialCommandMap
    case special of
      Just specialCmd -> runSpecial xs specialCmd
      Nothing -> lift $ system c >> return ()
    lift $ return ()
  -- Blank input
  _ -> lift $ return ()

matchChar :: CompleteState -> Char -> CommandInput
matchChar state c = case ord c of
  10  -> Run                          -- Newline
  13  -> Run                          -- Newline (Windows)
  8   -> Text $ Zip (drop 1 s) s'     -- Backspace (Windows)
  127 -> Text $ Zip (drop 1 s) s'     -- Backspace (Windows Ctr+backspace)
  6   -> Text $ Zip (reverse (complete state)) []
                                      -- Complete (Windows Ctr+F form feed)
  12  -> Execute "cls"                -- Clear screen (Ctr+L)
  3   -> Exit                         -- Exit (Windows Ctr+C)
  4   -> Exit                         -- EOF (Windows Ctr+D)
  224 -> PrepControlChar              -- Prep character
  75  -> ifControlPrepped $ left p    -- Left if prepped
  77  -> ifControlPrepped $ right p   -- Right if prepped
  72  -> if getControlPrepped state then HistoryBack else Text (push c p)
  80  -> if getControlPrepped state then HistoryForward else Text (push c p)
  x   -> Text $ push c p
  where p@(Zip s s') = getPrompt state
        ifControlPrepped r = Text $
          if getControlPrepped state then r else push c p

-- TODO factor out some of following
backHistory :: StateT CompleteState IO ()
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

forwardHistory :: StateT CompleteState IO ()
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
