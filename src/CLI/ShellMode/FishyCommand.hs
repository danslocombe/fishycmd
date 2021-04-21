{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module CLI.ShellMode.FishyCommand where

import CLI.Types
import CLI.State

import Text.Parsec
import System.Directory
import Data.List (intersperse, union)
import System.Environment
import System.Process (system)
import Control.Monad.IO.Class

-- We have an idea of 'fishy' commands that hold side effects
-- these are handled by the shell rather than external calls

data FishyCommand 
  = CD [String]
  | EXIT
  | LS [String]


tryGetFishyCommand :: String -> [String] ->  Maybe FishyCommand
tryGetFishyCommand "cd" args = Just $ CD args
tryGetFishyCommand "exit" _ = Just EXIT
tryGetFishyCommand "ls" args = Just $ LS args
tryGetFishyCommand _ _ = Nothing

joinArgs :: [String] -> String
joinArgs [] = ""
joinArgs [""] = ""
joinArgs xs = foldr1 (\x y -> x ++ " " ++ y) xs

runFishy :: FishyCommand -> FishyMonad Bool
runFishy cmd = do
  ifDebug $ putStrLn "Running fishy command..."
  case cmd of
    (CD args) -> (fishyCD $ joinArgs args) >> (return False)
    (LS args) -> liftIO (system $ "dir " ++ joinArgs args) >> return False
    EXIT -> return True

fishyCD :: String -> FishyMonad ()
fishyCD "" = liftIO $ (putStrLn =<< getCurrentDirectory)
fishyCD rawArg = do 
  ifDebug $ putStrLn ("CDing to \"" ++ rawArg ++ "\"")
  x <- liftIO $ sequence $ processCDArg rawArg
  case x of
    Just arg -> do
      let arg' = reverse $ dropWhile (==' ') $ reverse arg -- todo make whitespace stripping nicer
      ifDebug $ putStrLn ("CDing to \"" ++ arg' ++ "\"")
      exists <- liftIO $ doesDirectoryExist arg'
      if exists
      then liftIO $ setCurrentDirectory arg'
      else liftIO $ putStrLn "Error: fishy directory"
    Nothing -> liftIO $ putStrLn "Error: could not parse directory"

newtype DriveName = DriveName String deriving Show

data CDTarget = CDTarget (Maybe DriveName) [CDPart]

instance Show CDTarget where
  show (CDTarget d fs) = d' ++ joined
    where
      d' = case d of 
        Just (DriveName drive) -> drive ++ ":"
        Nothing -> ""
      parts = show <$> fs
      joined = concat $ intersperse "\\" parts

data CDPart = FolderName String
            | EnvVar String

instance Show CDPart where
  show (FolderName s) = s
  show (EnvVar s) = "%" ++ s ++ "%"

processCDArg :: String -> Maybe (IO String)
processCDArg a = (fmap $ fmap show) $ parseAndProcessTarget a'
  where
    a' = skipSwitch a

skipSwitch :: String -> String
skipSwitch s = case s' of 
    '/':'D':' ':xs -> xs
    '/':'d':' ':xs -> xs
    xs -> xs
  where
    s' = dropWhile (== ' ') s


parseAndProcessTarget :: String -> Maybe (IO CDTarget)
parseAndProcessTarget s = case parse parseTarget "" s of
  Right t -> Just $ processTarget t
  Left _ -> Nothing

processTarget :: CDTarget -> IO CDTarget
processTarget t = do
  let vs = extractVars t
  values <- lookupVars vs
  let replaced = foldl replaceVarTarget t values
  return replaced

replaceVar :: (String, String) -> CDPart -> CDPart
replaceVar (var,value) ev@(EnvVar v) = if v == var 
  then FolderName value
  else ev
replaceVar _ folder = folder

replaceVarTarget :: CDTarget -> (String, String) -> CDTarget
replaceVarTarget (CDTarget drive fp) (var,value) = CDTarget drive $ (replaceVar (var,value)) <$> fp

extractVars :: CDTarget -> [String]
extractVars (CDTarget _ fp) = foldl f [] fp
  where
    f xs (FolderName _) = xs
    f xs (EnvVar e) = union xs [e]

lookupVars :: [String] -> IO [(String, String)]
lookupVars vars = mapM f vars
  where
    f v = do
      value <- getEnv v
      return (v, value)

-- remove the /d switch we don't care about
stripCDSwitch :: String -> String
stripCDSwitch = undefined

parseTarget :: Stream s m Char => ParsecT s u m CDTarget
parseTarget = do
  md <- optionMaybe $ try parseDrive
  path <- parsePath
  return $ CDTarget md path

parseDrive :: Stream s m Char => ParsecT s u m DriveName
parseDrive = do
  c <- letter
  char ':'
  return $ DriveName [c]

parsePath :: Stream s m Char => ParsecT s u m [CDPart]
parsePath = parsePart `sepBy1` (oneOf "/\\")

parsePart :: Stream s m Char => ParsecT s u m CDPart
parsePart = try parseEnv <|> parseFolder

parseEnv :: Stream s m Char => ParsecT s u m CDPart
parseEnv = do
  char '%'
  v <- many letter
  char '%'
  return $ EnvVar v

parseFolder :: Stream s m Char => ParsecT s u m CDPart
parseFolder = FolderName <$> (many $ noneOf "%:/\\\n\t")
