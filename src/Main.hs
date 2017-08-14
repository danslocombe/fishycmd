module Main where

import Shell.State
  ( FishyState
  , cleanState
  , loadState
  , statePath
  )
import Shell.Update ( updateIOState )
import Shell.Command ( CommandProcessResult (..) )
  

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Semigroup ((<>))
import Data.List.Zipper (empty)
import System.Console.ANSI
import System.Directory
import System.Environment
import System.IO
import Options.Applicative hiding (empty)
import qualified Data.Map.Lazy as Map

data FishyOptions = FishyOptions
  { getClearHistoryOption :: Bool
  , getDebugOption :: Bool
  , getVerboseOption :: Bool } 

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

  -- Create state path if it's missing
  createDirectoryIfMissing True <$> statePath

  -- Draw entry header
  putStrLn entryString

  -- Load state from file unless clearHistory is set
  state <- if getClearHistoryOption options 
    then cleanState debug verbose [] Map.empty
    else loadState debug verbose

  -- Enter main loop
  let cpr = CommandProcessResult [] True False
  fishyLoop cpr state

fishyLoop :: CommandProcessResult -> FishyState -> IO ()
fishyLoop cpr state = do
  (cpr'@(CommandProcessResult _ _ exit), state')
    <- runStateT (updateIOState cpr) state
  if exit 
    then return () 
    else fishyLoop cpr' state'

entryString :: String
entryString = "\n\
\ ______________       ______         ______________  __________ \n\
\ ___  ____/__(_)_________  /______  ___  ____/__   |/  /__  __ \\\n\
\ __  /_   __  /__  ___/_  __ \\_  / / /  /    __  /|_/ /__  / / /\n\
\ _  __/   _  / _(__  )_  / / /  /_/ // /___  _  /  / / _  /_/ / \n\
\ /_/      /_/  /____/ /_/ /_/_\\__, / \\____/  /_/  /_/  /_____/  \n\
\                           /____/                             \n\
\ \n                  2ks Oneweek Hackathon 2017 - Dan Slocombe\n"
