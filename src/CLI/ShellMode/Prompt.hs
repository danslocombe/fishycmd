module CLI.ShellMode.Prompt where

import Data.Maybe (maybeToList)
import Data.List.Zipper hiding (insert)

-- Functions for traversing prompt

moveBlockLeft :: Zipper Char -> Zipper Char
moveBlockLeft (Zip back forwards) = ret
  where
    (xs, ys, ms) = splitReturnFirstNonTrivial back " /\\"
    ret = Zip ys (maybeToList ms ++ reverse xs ++ forwards)

moveBlockRight :: Zipper Char -> Zipper Char
moveBlockRight (Zip back forwards) = ret
  where
    (xs, ys, ms) = splitReturnFirstNonTrivial forwards " /\\"
    ret = Zip (maybeToList ms ++ reverse xs ++ back) ys

removeBlockLeft :: Zipper Char -> Zipper Char
removeBlockLeft (Zip back forwards) = ret
  where
    (_, ys, _) = splitReturnFirstNonTrivial back " /\\"
    ret = Zip ys forwards

splitReturnFirstNonTrivial :: Eq a => [a] -> [a] -> ([a], [a], Maybe a)
splitReturnFirstNonTrivial [] _ = ([], [], Nothing)
splitReturnFirstNonTrivial (x:xs) splits = 
  let (ys, ys', s) = splitReturnFirst xs splits
  in (x:ys, ys', s)

splitReturnFirst :: Eq a => [a] -> [a] -> ([a], [a], Maybe a)
splitReturnFirst [] _ = ([], [], Nothing)
splitReturnFirst (x:xs) splits = 
  if x `elem` splits
    then ([], xs, Just x)
    else let (ys, ys', s) = splitReturnFirst xs splits in
         (x:ys, ys', s)
