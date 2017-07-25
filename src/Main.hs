module Main where

import FileTries
import Trie
import TrieState
import Update
import Draw

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
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

main :: IO ()
main = do
  options <- execParser opts
  let debug = getDebugOption options
  sp <- statePath
  createDirectoryIfMissing True sp
  putStrLn entryString
  currentDir <- getCurrentDirectory
  files <- listDirectory currentDir
  -- putStrLn $ concatMap (((++)"\n") . stripQuotes . show) files
  let fileTries = buildTries files
  complete <- if clearHistory options 
    then cleanState debug [] fileTries
    else loadState debug fileTries
  repeaty complete

opts :: ParserInfo FishyOptions
opts = info (parseOptions <**> helper)
  ( fullDesc
  <> progDesc "Run the shell"
  <> header "FishyCMD - A wrapper for CMD" )

entryString :: String
-- entryString = "FishyCMD"
entryString = "\n\
\ ______________       ______         ______________  __________ \n\
\ ___  ____/__(_)_________  /______  ___  ____/__   |/  /__  __ \\\n\
\ __  /_   __  /__  ___/_  __ \\_  / / /  /    __  /|_/ /__  / / /\n\
\ _  __/   _  / _(__  )_  / / /  /_/ // /___  _  /  / / _  /_/ / \n\
\ /_/      /_/  /____/ /_/ /_/_\\__, / \\____/  /_/  /_/  /_____/  \n\
\                           /____/                             \n\
\ \n                  2ks Oneweek Hackathon 2017 - Dan Slocombe\n"
