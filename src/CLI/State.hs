module CLI.State 
    ( FishyState(..)
    , loadState
    , saveState
    , ifDebug
    , cleanState
    , storePath
    ) where

import Complete.String
import Complete.FileCompleter
import Complete.Git
import CLI.Types
import CLI.Helpers
import Corext.AliasCompleter
import Corext.Helpers

import Prelude

import Data.Serialize
import Data.List.Zipper
import System.Directory
import System.Environment
import System.IO
import Data.List.Split
import Control.Monad
import Text.Regex.Posix ((=~))
import System.Console.ANSI
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
  
-- Initialize a new 'clean' fishy state
cleanState :: Bool 
           -> Bool
           -> [StringTrie]
           -> Map.Map FilePath [StringTrie]
           -> [String]
           -> IO FishyState
cleanState debug verbose global local logs = do
  aliases <- parseAliases2 "aliases.pub"
  let aliases' = case aliases of  {
      Just x -> x;
      Nothing -> [];
  }
  -- putStrLn $ unlines $ show <$> aliases'
  handler <- (CompletionHandler global local
    <$> genPathyTries debug
    <*> loadGitCompletionHandler
    <*> createFileCompleter (FileCompleter "" []) ""
    <*> return 0)

  cd <- getCurrentDirectory 
  return $ FishyState
    { getCompletionHandler     = handler
    , getCachedCompletions     = (CompletionHandlerResult [] Red)
    , currentCompletion        = ""
    , getPrompt                = empty
    , lastPromptHeight         = 0
    , getControlPrepped        = False
    , getBufferedCommands      = []
    , getCurrentDir            = cd
    , getDebug                 = debug
    , getVerbose               = verbose
    , getHistoryStash          = Nothing
    , getHistoryLogs           = (Zip (reverse logs) [])
    , getHistoryIndex          = Nothing
    , getAliases               = aliases'
    }

genPathyTries :: Bool -> IO [StringTrie]
genPathyTries debug = do
  c <- inCorext
  logLine $ show c

  if c 
    then genCorextTries 
    else genPathTries debug

-- genAliases :: IO [(String, String)]
-- genAliases = do
  -- c <- inCorext
  -- if c then 

genCorextTries :: IO [StringTrie]
genCorextTries = putStrLn "Using CoreXT for completions" >>     
                 buildTries <$> parseAliases

genPathTries :: Bool -> IO [StringTrie]
genPathTries debug = do
  path <- getEnv "PATH"
  let pathSplit = splitOn ";" path
  validPaths <- filterM doesPathExist pathSplit
  files <- concat <$> mapM listDirectory validPaths
  let removeExe s = take (length s - 4) s
      files' = map removeExe $ filter (\x -> x =~ "(.+)\\.exe$") files

  when debug $ do
    logLine "-----------"
    logLine "Generating path tries"
    logLine "-----------"
    logLine "Path Locations:"
    logLine $ unlines validPaths
    logLine "-----------"
    logLine "Files:"
    logLine $ unlines files'
    logLine "-----------"

  return $ buildTries $ files'
  
stateFilename :: String
stateFilename = "trie.file"

serializableFromFishy :: FishyState -> SerializableState
serializableFromFishy fs =
  let ch = getCompletionHandler fs
  in SerializableState 
    { serializedHistoryTries = getHistoryTries ch
    , serializedLocalizedTries = getLocalizedHistoryTries ch
    , serializedHistoryLogs = toList $ getHistoryLogs fs
    }

-- Save state by serializing history tries
saveState :: FishyState -> IO ()
saveState s = do
  filepath <- storePath
  let sstate = serializableFromFishy s
  BS.writeFile (filepath ++ stateFilename) $ encode sstate

-- Load state by deserializing history tries
loadState :: Bool -> Bool -> IO FishyState
loadState debug verbose = do
  filepath <- storePath
  let readPath = filepath ++ stateFilename

  when verbose $
    putStr $ "Reading state from: \"" ++ readPath ++ "\"..."

  whenM (not <$> doesFileExist readPath) $ do
      when verbose $ putStr "\nCreating file..."
      createDirectoryIfMissing True filepath
      handle <- openFile readPath ReadWriteMode
      hClose handle
      when verbose $ putStrLn "Done"

  d <- BS.readFile $ readPath
  let d' = decode d
  case d' of
    (Right (SerializableState h l logs)) -> do
      when verbose $ putStrLn "Success!"
      cleanState debug verbose h l logs
    (Left err) -> do
      when verbose $ putStrLn $ "Failed! Creating blank state" ++ err
      cleanState debug verbose [] Map.empty []
