module CLI.ShellMode.CompletionHandler where

import Complete
import Complete.FileCompleter
import Complete.String
import Complete.Trie (insertTrie)
import Complete.Types
import CLI.Types

import Control.Monad
import Data.Maybe
import Data.List.Zipper hiding (insert)
import Data.List.Split
import qualified Data.Map.Lazy as Map
import System.Console.ANSI
import System.Directory

-- Try get first completion or give empty results
firstCompletionResult :: CompletionHandlerResult -> (Completion Char, Color)
firstCompletionResult (CompletionHandlerResult xs c) =
  (fromMaybe (Completion [] 0) $ listToMaybe xs, Red)

-- Get (n mod k)th completion where k is the number of completions
cycledCompletionResult :: CompletionHandler -> CompletionHandlerResult ->  (StringCompletion, Color)
cycledCompletionResult handler (CompletionHandlerResult xs c) 
  = (fromMaybe (Completion [] 0) $ xs !%! n, Red)
  where
    n = getCycle handler

updateCompletionHandler :: CompletionHandler ->
                           Zipper Char ->
                           FilePath ->
                           [String] ->
                           IO CompletionHandler

updateCompletionHandler old prompt dir newCommands = do
  fileCompleter <- createFileCompleter (getFileCompleter old) (toList prompt)
  globalNewCommands <- filterM (isGlobalCommand dir) newCommands
  let addToTrie :: [String] -> [StringTrie] -> [StringTrie]
      addToTrie commands trie = foldl (flip insertTrie) trie commands

      global = addToTrie globalNewCommands $ getHistoryTries old

      localTriesOld = getLocalizedHistoryTries old
      localTrie = Map.findWithDefault [] dir localTriesOld
      localTrie' = addToTrie newCommands localTrie
      localTriesNew = Map.insert dir localTrie' localTriesOld
  return old 
    { getFileCompleter         = fileCompleter
    , getHistoryTries          = global 
    , getLocalizedHistoryTries = localTriesNew
    -- We leave path completions alone for now
  }

getCurrentCompletion :: CompletionHandler -> String -> String -> CompletionHandlerResult
getCurrentCompletion handler prefix currentDir = case length splitS of
  -- Don't do anything for empty string
  0 -> singletonResult
  -- For a single 'word' use all tries
  1 -> getCurrentCompletionInner handler prefix currentDir allCompleterNames
  -- For multiple 'words'
  _ -> let (CompletionHandlerResult hs _ ) 
             = getCurrentCompletionInner handler prefix currentDir historyCompleterNames
           (CompletionHandlerResult fs _ )
             = getCurrentCompletionInner handler endPrefix currentDir fileCompleterNames
           -- Don't complete on files if in quotes
           fs' = if inQuotes prefix then [] else fs
           fs'' = fmap (\(Completion c s) -> Completion (prefix++(drop n c)) s) fs'
        in CompletionHandlerResult (hs ++ fs'') Red

  where
    allCompleterNames = 
      [ NameLocalHistoryCompleter
      , NameFileCompleter
      , NameGlobalHistoryCompleter
      , NamePathCompleter ]

    historyCompleterNames =
      [ NameLocalHistoryCompleter
      , NameGlobalHistoryCompleter ]

    fileCompleterNames =
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
    hasDesiredName (FishyCompleterResult _ name)
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

-- Wrapping list indexing
-- [0, 5, 2] !%! 4 = [0, 5, 2] !! 1 = 5
(!%!) :: [a] -> Int -> Maybe a
(!%!) xs i =  liftM (\ys -> ys !! (i `tomAbs` n)) xs'
  where 
    xs' = guard (not (null xs)) >> Just xs
    n = length xs
    tomAbs :: (Integral a, Num a) => a -> a -> a
    tomAbs = (abs .) . mod


inQuotes :: String -> Bool
-- We say that the user is currently typing something in quotes if there is 
-- an odd number of " chars
inQuotes s = (length (filter (=='"') s) `mod` 2) /= 0


-- Test if a command refers to any local files
-- if it does we don't want to add it to the global entry
isGlobalCommand :: FilePath -> String -> IO Bool
isGlobalCommand dir s = do
  let splitS = splitOn " " s
  localFilenames <- mapM
    (\f ->
      if length f > 3 && f !! 1 == ':' && (f !! 2 == '\\' || f !! 2 == '/')
        then return False -- We assume this is C:\apwfjap\ so not a local file
        else do
          isFile <- doesFileExist $ dir ++ "\\" ++ f
          -- putStrLn $ dir ++ "\\" ++ f ++ " " ++ show isFile
          isDir <- doesDirectoryExist (dir ++ "\\" ++ f ++ "\\")
          -- putStrLn $ dir ++ "\\" ++ f ++ " " ++ show isDir
          return $ isFile || isDir
     ) splitS
  return $ not $ any id $ localFilenames
