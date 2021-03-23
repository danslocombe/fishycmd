module Main where

import CLI.State
  ( cleanState
  , loadState
  , storePath
  )

import CLI.Loop
import CLI.Modes
import CLI.Types
import Test.Trie
  
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Semigroup ((<>))
import System.Directory
import Options.Applicative hiding (empty)
import qualified Data.Map.Lazy as Map

data FishyOptions = FishyOptions
  { getClearHistoryOption :: Bool
  , getDebugOption :: Bool
  , getVerboseOption :: Bool
  , getTestOption :: Bool
  } 

parseOptions :: Parser FishyOptions
parseOptions = FishyOptions
  <$> switch
     ( long "clear-history"
    <> short 'c'
    <> help "Clear command history" )
  <*> switch
     ( long "debug"
    <> short 'd'
    <> help "Set debug mode" )
  <*> switch
     ( long "verbose"
    <> short 'v'
    <> help "Set verbose mode" )
  <*> switch
     ( long "test"
    <> short 't'
    <> help "Run tests" )

opts :: ParserInfo FishyOptions
opts = info (parseOptions <**> helper)
  ( fullDesc
  <> progDesc "Run the shell"
  <> header "FishyCMD - A wrapper for CMD" )

main :: IO ()
main = do
  -- Fetch argvs
  options <- execParser opts
  let debug = getDebugOption options
      verbose = getVerboseOption options
      test = getTestOption options

  when test runTests

  -- Create state path if it's missing
  createDirectoryIfMissing True <$> storePath

  -- Draw entry header
  putStrLn entryString

  -- Load state from file unless clearHistory is set
  initState <- if getClearHistoryOption options 
    then cleanState debug verbose [] Map.empty []
    else loadState debug verbose

  runStateT (loop (lookupMode ShellMode)) initState
  return ()

entryString :: String
entryString = "Fishy v0.4.04     >°))))<"

-- Release notes
-- 0.4.04: fix parsing of xap aliases

entryString2 :: String
entryString2 = "\n\
\ ______________       ______         ______________  __________ \n\
\ ___  ____/__(_)_________  /______  ___  ____/__   |/  /__  __ \\\n\
\ __  /_   __  /__  ___/_  __ \\_  / / /  /    __  /|_/ /__  / / /\n\
\ _  __/   _  / _(__  )_  / / /  /_/ // /___  _  /  / / _  /_/ / \n\
\ /_/      /_/  /____/ /_/ /_/_\\__, / \\____/  /_/  /_/  /_____/  \n\
\                           /____/                             \n\
\ \n                  2ks Oneweek Hackathon 2017 - Dan Slocombe\n"
