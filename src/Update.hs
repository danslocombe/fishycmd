{-# LANGUAGE ForeignFunctionInterface #-}

module Update where

import Complete
import Draw
import FileCompleter
import StringTries
import Trie
import TrieState

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
import qualified Data.Map.Lazy as Map

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

rebuildFileCompleter :: StateT FishyState IO ()
rebuildFileCompleter = do
  state <- get
  fileCompleter <- lift $ createFileCompleter "" (toList $ getPrompt state)
  put $ state {getFileCompleter  = fileCompleter}

updateIOState :: StateT FishyState IO Bool
updateIOState = do
  -- TODO THIS IS REALLY SLOW
  -- don't do every update
  rebuildFileCompleter
  -- Scrape info from state
  state <- get
  let p = getPrompt state
      historyLogs = getHistoryLogs state
      s = toList p
  currentDir <- lift $ getCurrentDirectory

  -- Draw completion then yield for next char
  lift $ drawCompletion state
  c <- lift getHiddenChar

  -- This shows the ascii character of the input
  ifDebug (putStrLn $ show $ ord c)

  case matchChar state currentDir c of 
    -- Update using new prompt state
    Text prompt -> do
      lift $ setCursorColumn 0
      lift clearFromCursorToLineEnd
      put state {getPrompt = prompt, getControlPrepped = False}
      return False

    -- Exit the shell
    Exit -> return True

    -- Run what is entered by user
    Run -> do
      dirToInsert <- lift $ getCurrentDirectory
      exitcode <- execCommand s
      state' <- get
      let historyTries = insertCW s $ getHistoryTries state'
          localizedTries = getLocalizedHistoryTries state'
          localizedTrie = Map.findWithDefault [] dirToInsert localizedTries
          localizedTrie' = insertCW s $ localizedTrie
          localizedTries' = Map.insert dirToInsert localizedTrie' localizedTries
      put $ state' { getHistoryTries = historyTries
        , getLocalizedHistoryTries = localizedTries'
        , getPrompt = empty
        , getHistoryLogs = push s historyLogs }
      lift $ saveState state
      return False

    -- Execute some other command
    Execute command -> do
      exitcode <- lift $ system command
      state' <- get
      let state'' = state { getHistoryLogs = push s historyLogs }
      put state''
      lift $ return False

    -- Some inputs (up, down, etc) are represented by two characters
    -- The 'control character' then some other, this is called on input of the control char
    PrepControlChar -> do
      put state {getControlPrepped = True}
      lift $ return False

    -- Move back through history
    HistoryBack -> do
      backHistory
      state' <- get
      ifDebug $ putStrLn ("\nBack History: " ++ show (getHistoryLogs state'))
      lift $ return False

    -- Move forwards through history
    HistoryForward -> do
      forwardHistory
      state' <- get
      ifDebug $ putStrLn ("\nForwards History: " ++ show (getHistoryLogs state'))
      lift $ return False

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
    let special = lookup x specialCommandMap
    case special of
      Just specialCmd -> runSpecial xs specialCmd
      Nothing -> lift $ system c >> return ()
    lift $ putStr "\n"
  -- Blank input
  _ -> lift $ return ()

-- Hardcoded windows inputs
matchChar :: FishyState -> String -> Char -> CommandInput
matchChar state currentDir c = case ord c of
  10  -> Run                          -- Newline
  13  -> Run                          -- Newline (Windows)
  8   -> Text $ Zip (drop 1 s) s'     -- Backspace (Windows)
  127 -> Text $ Zip (drop 1 s) s'     -- Backspace (Windows Ctr+backspace)
  6   -> Text $ Zip (reverse (fishyComplete state currentDir)) []
                                      -- Complete (Windows Ctr+F form feed)
  9   -> Text $ Zip (reverse (fishyPartialComplete state currentDir)) []
                                      -- Partial complete (Tab)
  12  -> Execute "cls"                -- Clear screen (Ctr+L)
  3   -> Exit                         -- Exit (Windows Ctr+C)
  4   -> Exit                         -- EOF (Windows Ctr+D)
  224 -> PrepControlChar              -- Prep character
  75  -> ifControlPrepped $ left p    -- Left if prepped
  77  -> ifControlPrepped $ right p   -- Right if prepped
  72  -> if getControlPrepped state then HistoryBack else Text (push c p)
  80  -> if getControlPrepped state then HistoryForward else Text (push c p)
  71  -> ifControlPrepped $ Zip [] (toList p) -- Home
  79  -> ifControlPrepped $ Zip (reverse $ toList p) [] -- End
  x   -> Text $ push c p
  where p@(Zip s s') = getPrompt state
        ifControlPrepped r = Text $
          if getControlPrepped state then r else push c p

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
