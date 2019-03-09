{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module CLI.Helpers where

import CLI.Types
import Complete.Types

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.RWS.Class
import Data.List.Zipper

import System.Environment

-- f Only if x
infix 0 <~?
(<~?) :: (Monad m) => m a -> m Bool -> m ()
f <~? x = x >>= (\y -> if y then f >> return () else return ())

-- f Only if x
infix 0 <-?
(<-?) :: (Applicative f) => f () -> Bool -> f ()
f <-? x = if x then f else pure ()

-- if x then f
infix 0 ?~>
(?~>) :: (Monad m) => m Bool -> m a -> m ()
(?~>) = flip (<~?)

-- fi x then f
infix 0 ?->
(?->) :: (Applicative f) => Bool -> f() -> f ()
(?->) = flip (<-?)

(+^+) :: Monad m => m [a] -> m [a] -> m [a]
(+^+) = liftM2 (++)

-- Run some arbitrary IO if we are running in debug mode
ifDebug :: IO () -> FishyMonad ()
ifDebug f = do (liftIO f) <~? getDebug <$> get

storePath :: IO FilePath
storePath = do
  appdata <- getEnv "APPDATA" 
  return $ appdata ++ "\\fishycmd\\"

logLine :: String -> IO ()
logLine s = do
  sp <- storePath
  let logpath = sp ++ "fish.log"
  appendFile logpath (s ++ "\n")

logCompletions :: String -> String -> CompletionHandlerResult -> IO ()
logCompletions prefix cd completions = do
  let f :: CompletionHandlerResult -> [String]
      f (CompletionHandlerResult cs _) 
        = map (\(Completion x w) -> x ++ "[" ++ show w ++ "]") cs
  logLine $ prefix 
    ++ ", "
    ++ cd 
    ++ " ->\n"
    ++ (unlines (f completions))
    ++ "\n\n"

(!?!) :: [a] -> Int -> Maybe a
(!?!) [] _ = Nothing
(!?!) (x:_) 0 = Just x
(!?!) (x:xs) n = xs !?! (n-1)

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:xs) = Just xs

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
