module Complete.Git where

import Complete.String
import Complete.Types()

import System.Process
import Data.List.Split (splitOn)
import Data.List (intersperse)
import Data.Maybe (catMaybes)

data GitCompletionHandler = GitCompletionHandler
  { getBranchTries :: [StringTrie]
  } deriving (Show)

loadGitCompletionHandler :: IO GitCompletionHandler
loadGitCompletionHandler = do
    -- Setup flags for process

    -- immitating fish
    -- iterate over refs instead of calling "git branch"
    -- faster and avoids localised "detached HEAD" messages.
    let refCommand = "git for-each-ref --format='%(refname)' refs/heads/ refs/remotes/"
    refs <- readCreateProcess (shell refCommand) ""

    let additionalCompletions = ["HEAD"]
    let input = additionalCompletions ++ (catMaybes $ parseRef <$> splitOn "\n" refs)
    --sequence $ putStrLn <$> input

    return GitCompletionHandler {
        getBranchTries = buildTries input
    }

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

isGitCommand :: String -> Bool
isGitCommand ('g':'i':'t':xs) = case xs of 
    (' ':_) -> True
    [] -> True
    _ -> False
isGitCommand _ = False

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