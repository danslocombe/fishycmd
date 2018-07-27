{-# LANGUAGE FlexibleContexts #-}
module Shell.FishyCommand where

import Shell.Types
import Shell.State

import Text.Parsec
import Text.Parsec.Char
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import System.Directory
import Data.List (intersperse, union)
import System.Environment

-- We have an idea of 'fishy' commands that hold side effects
-- these are handled by the shell rather than external calls

data FishyCommand = CD | EXIT

fishyCommandMap = 
  [ ("cd", CD)
  , ("exit", EXIT)
  ]

runFishy :: [String] -> FishyCommand -> StateT FishyState IO Bool
runFishy args cmd = do
  ifDebug $ putStrLn "Running fishy command..."
  case cmd of
    CD -> let arg = (case args of
                       [] -> ""
                       [""] -> ""
                       xs -> foldr1 (\x y -> x ++ " " ++ y) xs)
      in fishyCD arg >> (lift $ return False)
    EXIT -> lift $ return True

fishyCD :: String -> StateT FishyState IO ()
fishyCD "" = lift $ (putStrLn =<< getCurrentDirectory)
fishyCD rawArg = do 
  ifDebug $ putStrLn ("Cd ing to \"" ++ rawArg ++ "\"")
  x <- lift $ sequence $ processCDArg rawArg
  case x of
    Just arg -> do
      exists <- lift $ doesPathExist rawArg
      if exists
      then do
        lift $ setCurrentDirectory rawArg
        dir <- lift $ getCurrentDirectory
        lift $ return ()
      else lift $ putStrLn "Error: fishy directory"
    Nothing -> lift $ putStrLn "Error: could not parse directory"

newtype DriveName = DriveName String deriving Show

data CDTarget = CDTarget (Maybe DriveName) [CDPart]

instance Show CDTarget where
  show (CDTarget d fs) = d' ++ joined
    where
      d' = case d of 
        Just (DriveName drive) -> drive ++ ":"
        Nothing -> ""
      parts = show <$> fs
      joined = concat $ intersperse "/" parts

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
