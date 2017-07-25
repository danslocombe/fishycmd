module Main where

import Draw
import FileTries
import Trie
import TrieState
import Update

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

data FishyOptions = FishyOptions
  { clearHistory :: Bool
  , getDebugOption :: Bool }

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

  -- Create state path if it's missing
  createDirectoryIfMissing True <$> statePath

  -- Draw entry header
  putStrLn entryString

  -- For now files trie is static
  fileTries <- buildTries <$> (listDirectory =<< getCurrentDirectory)

  -- Load state from file unless clearHistory is set
  state <- if clearHistory options 
    then cleanState debug [] fileTries
    else loadState debug fileTries

  -- Enter main loop
  fishyLoop state

fishyLoop :: FishyState -> IO ()
fishyLoop state = do
  (res, state') <- runStateT updateIOState state
  if res then return () else fishyLoop state'

entryString :: String
entryString = "\n\
\ ______________       ______         ______________  __________ \n\
\ ___  ____/__(_)_________  /______  ___  ____/__   |/  /__  __ \\\n\
\ __  /_   __  /__  ___/_  __ \\_  / / /  /    __  /|_/ /__  / / /\n\
\ _  __/   _  / _(__  )_  / / /  /_/ // /___  _  /  / / _  /_/ / \n\
\ /_/      /_/  /____/ /_/ /_/_\\__, / \\____/  /_/  /_/  /_____/  \n\
\                           /____/                             \n\
\ \n                  2ks Oneweek Hackathon 2017 - Dan Slocombe\n"
