module CLI.ShellMode.Prompt where

import CLI.Types
import Data.Maybe (maybeToList)
import Data.List.Zipper hiding (insert)

-- Functions for treating zipper as two stacks

pushTopZipper :: a -> Zipper a -> Zipper a
pushTopZipper y (Zip xs ys) = Zip xs (y : ys)

pushBotZipper :: a -> Zipper a -> Zipper a
pushBotZipper x (Zip xs ys) = Zip (x : xs) ys

popTopZipper :: Zipper a -> (Zipper a, Maybe a)
popTopZipper (Zip xs (y:ys)) = (Zip xs ys, Just y)
popTopZipper (Zip xs []) = (Zip xs [], Nothing)

popBotZipper :: Zipper a -> (Zipper a, Maybe a)
popBotZipper (Zip (x:xs) ys) = (Zip xs ys, Just x)
popBotZipper (Zip [] ys) = (Zip [] ys, Nothing)

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:xs) = Just xs

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
