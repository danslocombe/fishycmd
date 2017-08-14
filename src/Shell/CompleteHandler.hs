module Shell.CompleteHandler
  ( CompletionHandler
  , CompletionHandlerResult (..)
  , getCurrentCompletion
  , updateCompletionHandler
  , cycleCompletionHandler
  , revCycleCompletionHandler
  , resetCompletionHandler
  , firstCompletionResult
  , (!%!)
  ) where

import Complete
import Complete.FileCompleter
import Complete.String
import Complete.Completer
import Complete.Types
import Shell.Types

import Control.Monad
import Data.Maybe
import Data.List.Zipper hiding (insert)
import qualified Data.Map.Lazy as Map
import System.Console.ANSI


firstCompletionResult :: CompletionHandlerResult -> (Completion Char, Color)
firstCompletionResult (CompletionHandlerResult xs c) = (fromMaybe (Completion []) $ listToMaybe xs, Red)

updateCompletionHandler :: CompletionHandler ->
                           Zipper Char ->
                           FilePath ->
                           [String] ->
                           IO CompletionHandler

-- TODO save state
updateCompletionHandler old prompt dir newCommands = do
  fileCompleter <- createFileCompleter (getFileCompleter old) (toList prompt)
  return old 
    { getFileCompleter         = fileCompleter
    , getHistoryTries          = global 
    , getLocalizedHistoryTries = local
    -- We leave path completions alone for now
  }
  where
    addToTrie :: [StringTrie] -> [StringTrie]
    addToTrie trie = foldl (flip insert) trie newCommands
    global = addToTrie $ getHistoryTries old
    localTries = getLocalizedHistoryTries old
    localTrie = Map.findWithDefault [] dir localTries
    localTrie' = addToTrie localTrie
    local = Map.insert dir localTrie' localTries

getCurrentCompletion :: CompletionHandler ->
                        String ->
                        String ->
                        [CompleterName] ->
                        CompletionHandlerResult

getCurrentCompletion 
  handler
  prefix
  currentDir
  queryTargets
  = toHandlerResult res
  where
    -- Convert to StringCompleters
    completers = allCompleters handler currentDir
    -- Get completions
    rs = allCompletions completers prefix

    -- Functions for filtering
    filterResults :: FishyCompleterResult -> Maybe StringCompleterResult
    filterResults cr@(FishyCompleterResult cs _)
      = guard (hasDesiredName cr) >> Just cs
    hasDesiredName :: FishyCompleterResult -> Bool
    hasDesiredName (FishyCompleterResult cr name)
      = elem name queryTargets

    -- Select only the completions with names we want
    res :: [StringCompleterResult]
    res = (catMaybes (map filterResults rs))

    toHandlerResult :: [StringCompleterResult] -> CompletionHandlerResult
    --toHandlerResult (CompleterResult xs color) = 
        --CompletionHandlerResult xs color
    toHandlerResult cs = CompletionHandlerResult all Red
      where all = concatMap (\(CompleterResult xs _) -> xs) cs

allCompleters :: CompletionHandler -> String -> [StringCompleter]
allCompleters handler currentDir = 
  [ StringCompleter local                    NameLocalHistoryCompleter
  , StringCompleter (getFileCompleter handler) NameFileCompleter
  , StringCompleter (getHistoryTries  handler) NameGlobalHistoryCompleter
  , StringCompleter (getPathTries     handler) NamePathCompleter]
  where local = Map.findWithDefault [] currentDir $ getLocalizedHistoryTries handler

-- Again, should use lenses
cycleCompletionHandler :: CompletionHandler -> CompletionHandler
cycleCompletionHandler handler = handler {getCycle = (getCycle handler) + 1}

revCycleCompletionHandler :: CompletionHandler -> CompletionHandler
revCycleCompletionHandler handler = handler {getCycle = (getCycle handler) - 1}

resetCompletionHandler :: CompletionHandler -> CompletionHandler
resetCompletionHandler handler = handler {getCycle = 0}

tomAbs :: (Integral a, Num a) => a -> a -> a
tomAbs = (abs .) . mod

(!%!) :: [a] -> Int -> Maybe a
(!%!) xs i =  liftM (\ys -> ys !! (i `tomAbs` n)) xs'
  where 
    xs' = guard (not (null xs)) >> Just xs
    n = length xs
