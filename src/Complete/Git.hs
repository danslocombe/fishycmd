{-# LANGUAGE RecordWildCards #-}

module Complete.Git where

import Complete.String
import Complete.Types()

import System.Process
import Data.List.Split (splitOn)
import Data.List (intersperse)
import Data.Maybe (catMaybes)
import System.Directory

data GitCompletionHandler = GitCompletionHandler
  { getBranchTries :: [StringTrie]
  , getInvalidated :: Bool
  } deriving (Show)

loadGitCompletionHandler :: IO GitCompletionHandler
loadGitCompletionHandler = do
  inRepo <- inGitRepo
  if inRepo
    then createGitCompletionHandler
    else return GitCompletionHandler
      { getBranchTries = []
      , getInvalidated = False
      }

createGitCompletionHandler :: IO GitCompletionHandler
createGitCompletionHandler = do
    -- Setup flags for process

    -- immitating fish
    -- iterate over refs instead of calling "git branch"
    -- faster and avoids localised "detached HEAD" messages.
    --putStrLn "Rebuilding git completion handler"
    let refCommand = "git for-each-ref --format='%(refname)' refs/heads/ refs/remotes/"
    refs <- readCreateProcess (shell refCommand) ""

    let additionalCompletions = ["HEAD"]
    let input = additionalCompletions ++ (catMaybes $ parseRef <$> splitOn "\n" refs)
    --sequence $ putStrLn <$> input

    return GitCompletionHandler
      { getBranchTries = buildTries input
      , getInvalidated = False
      }

rebuildIfNeeded :: GitCompletionHandler -> String -> IO GitCompletionHandler
rebuildIfNeeded git@GitCompletionHandler{..} s = if not getInvalidated && isGitCommand s
  then return git
  else loadGitCompletionHandler

invalidate :: GitCompletionHandler -> GitCompletionHandler
invalidate g = g { getInvalidated = True }

inGitRepo :: IO Bool
inGitRepo = do
  let command = "git rev-parse --is-inside-work-tree"
  (_exitCode, stdOut, _stdErr) <- readCreateProcessWithExitCode (shell command) ""
  return $ startsWithThenWhitespace "true" stdOut

trimRef :: String -> String
trimRef = f . f
  where f = reverse . dropWhile (== '\'')

parseRef :: String -> Maybe String
parseRef x = case xs of
        ("refs":"heads":ys) -> Just $ reconstruct ys
        ("refs":"remotes":ys) -> Just $ reconstruct ys
        _ -> Nothing
    where
        xs = splitOn "/" $ trimRef x
        reconstruct = concat . (intersperse "/")

startsWithThenWhitespace :: String -> String -> Bool
startsWithThenWhitespace [] [] = True
startsWithThenWhitespace [] (' ':_) = True
startsWithThenWhitespace [] ('\r':_) = True
startsWithThenWhitespace [] ('\n':_) = True
startsWithThenWhitespace [] _ = False
startsWithThenWhitespace (_:_) [] = False
startsWithThenWhitespace (x:xs) (y:ys) = x == y && startsWithThenWhitespace xs ys

isGitCommand :: String -> Bool
isGitCommand = startsWithThenWhitespace "git"

shouldCompleteBranch :: [String] -> Maybe (String, String)
shouldCompleteBranch (x:token:xs) = case token of
    "checkout" -> retBase
    "log" -> retBase
    "diff" -> retBase
    "merge" -> retBase
    "show" -> retBase
    _ -> Nothing
    where
        retBase = Just (x ++ " " ++ token ++ " ", reconstructed)
        reconstructed = concat $ intersperse " " xs
shouldCompleteBranch _ = Nothing

--shouldCompleteBranch :: [String]
--    "checkout"
--    "log"
--    "diff"
--    "rebase"
--