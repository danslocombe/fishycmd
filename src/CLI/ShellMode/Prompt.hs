module CLI.ShellMode.Prompt where

import CLI.Types
import CLI.Helpers

import Data.Maybe (maybeToList)
import Data.List.Zipper hiding (insert)

-- Functions for traversing prompt

moveBlockLeft :: Zipper Char -> Zipper Char
moveBlockLeft z@(Zip back forwards) = ret
  where
    (xs, ys, ms) = splitReturnFirstNonTrivial back " /\\"
    ret = Zip ys (maybeToList ms ++ reverse xs ++ forwards)

moveBlockRight :: Zipper Char -> Zipper Char
moveBlockRight z@(Zip back forwards) = ret
  where
    (xs, ys, ms) = splitReturnFirstNonTrivial forwards " /\\"
    ret = Zip (maybeToList ms ++ reverse xs ++ back) ys

removeBlockLeft :: Zipper Char -> Zipper Char
removeBlockLeft z@(Zip back forwards) = ret
  where
    (xs, ys, ms) = splitReturnFirstNonTrivial back " /\\"
    ret = Zip ys forwards

splitReturnFirstNonTrivial :: Eq a => [a] -> [a] -> ([a], [a], Maybe a)
splitReturnFirstNonTrivial [] splits = ([], [], Nothing)
splitReturnFirstNonTrivial (x:xs) splits = 
  let (ys, ys', s) = splitReturnFirst xs splits
  in (x:ys, ys', s)

splitReturnFirst :: Eq a => [a] -> [a] -> ([a], [a], Maybe a)
splitReturnFirst [] splits = ([], [], Nothing)
splitReturnFirst (x:xs) splits = 
  if x `elem` splits
    then ([], xs, Just x)
    else let (ys, ys', s) = splitReturnFirst xs splits in
         (x:ys, ys', s)
