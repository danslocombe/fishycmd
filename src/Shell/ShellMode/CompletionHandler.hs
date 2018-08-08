module Shell.ShellMode.CompletionHandler where

import Complete
import Complete.FileCompleter
import Complete.String
import Complete.Trie (insertTrie)
import Complete.Types
import Shell.Types

import Control.Monad
import Data.Maybe
import Data.List.Zipper hiding (insert)
import Data.List.Split
import qualified Data.Map.Lazy as Map
import System.Console.ANSI

firstCompletionResult :: CompletionHandlerResult -> (Completion Char, Color)
firstCompletionResult (CompletionHandlerResult xs c) = (fromMaybe (Completion [] 0) $ listToMaybe xs, Red)

updateCompletionHandler :: CompletionHandler ->
                           Zipper Char ->
                           FilePath ->
                           [String] ->
                           IO CompletionHandler

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
    addToTrie trie = foldl (flip insertTrie) trie newCommands
    global = addToTrie $ getHistoryTries old
    localTries = getLocalizedHistoryTries old
    localTrie = Map.findWithDefault [] dir localTries
    localTrie' = addToTrie localTrie
    local = Map.insert dir localTrie' localTries

getCurrentCompletion :: CompletionHandler -> String -> String -> CompletionHandlerResult
getCurrentCompletion handler prefix currentDir = case length splitS of
  -- Don't do anything for empty string
  0 -> singletonResult
  -- For a single 'word' use all tries
  1 -> getCurrentCompletionInner handler prefix currentDir allCompleters
  -- For multiple 'words'
  _ -> let (CompletionHandlerResult hs _ ) 
             = getCurrentCompletionInner handler prefix currentDir historyCompleters
           (CompletionHandlerResult fs _ )
             = getCurrentCompletionInner handler endPrefix currentDir fileCompleters
           fs' = fmap (\(Completion c s) -> Completion (prefix++(drop n c)) s)  fs
        in CompletionHandlerResult (hs ++ fs') Red

  where
    allCompleters = 
      [ NameLocalHistoryCompleter
      , NameFileCompleter
      , NameGlobalHistoryCompleter
      , NamePathCompleter ]

    historyCompleters =
      [ NameLocalHistoryCompleter
      , NameGlobalHistoryCompleter ]

    fileCompleters =
      [ NameFileCompleter
      , NamePathCompleter ]

    splitS :: [String]
    splitS = splitOn " " prefix

    endPrefix = last splitS
    n = length endPrefix

    singletonResult = CompletionHandlerResult [] Red

getCurrentCompletionInner :: CompletionHandler ->
                        String ->
                        String ->
                        [CompleterName] ->
                        CompletionHandlerResult

getCurrentCompletionInner 
  handler
  prefix
  currentDir
  queryTargets
  = toHandlerResult res
  where
    -- Get all completers for the current directory
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

    -- Select only results from completers we care about
    res :: [StringCompleterResult]
    res = (catMaybes (map filterResults rs))

    -- Add red color
    toHandlerResult :: [StringCompleterResult] -> CompletionHandlerResult
    --toHandlerResult (CompleterResult xs color) = 
        --CompletionHandlerResult xs color
    toHandlerResult cs = CompletionHandlerResult all Red
      where all = concatMap (\(CompleterResult xs _) -> xs) cs

allCompleters :: CompletionHandler -> String -> [StringCompleter]
allCompleters handler currentDir = 
  [ StringCompleter local                      NameLocalHistoryCompleter
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
