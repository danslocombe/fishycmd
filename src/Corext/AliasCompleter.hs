{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Corext.AliasCompleter where

import Safe
import Data.Maybe
import Data.List.Split hiding (oneOf)
import Data.List (intersperse)
import System.Environment

import Text.Parsec
import Text.Parsec.Char
import Data.List

import Shell.Types

inetroot :: IO (Maybe String)
inetroot = lookup "INETROOT" <$> getEnvironment

inCorext :: IO Bool
inCorext = isJust <$> inetroot

-- Assume we are in a corext env with %INETROOT% defined
aliasPath :: IO FilePath
aliasPath = do
  root <- inetroot >>= \s -> case s of
    Just x -> return x
    Nothing -> error $ "Error, not running in CoreXT!\n"
                    ++ "Tried to find value of %INETROOT% but it wasn't set"
  return $ root ++ "\\build\\aliases\\aliases.pub"

parseAliases :: IO [String]
parseAliases = do
  aliasesRaw <- splitOn "\n" <$> (readFile =<< aliasPath)
  let aliases = map (splitOn "=") aliasesRaw
      aliasKeysWhitespace = mapMaybe headMay aliases
      aliasKeys :: [String] =  mapMaybe (headMay . (splitOn " ")) aliasKeysWhitespace
  putStrLn $ "CoreXT keys : " ++ (concat $ intersperse " " aliasKeys)
  return aliasKeys

parseAliases2 :: String -> IO (Maybe [Alias])
parseAliases2 filename = do
  aliasesRaw <- readFile =<< aliasPath
  case parse parseAllAliasDefs filename aliasesRaw of
    Right x -> return $ Just x
    _ -> return Nothing

parseAliasArgWildCard :: Stream s m Char => ParsecT s u m AliasElem
parseAliasArgWildCard = string "$*" >> return AliasArgWildCard

parseAliasArg :: Stream s m Char => ParsecT s u m AliasElem
parseAliasArg = do
  string "$"
  n <- read . return <$> digit
  return $ AliasArg n

parseAliasStr :: Stream s m Char => ParsecT s u m AliasElem
parseAliasStr = AliasStr <$> (many1 $ noneOf "$\n")

parseAliasElem :: Stream s m Char => ParsecT s u m AliasElem
parseAliasElem = choice $ try <$>
  [ parseAliasArgWildCard
  , parseAliasArg
  , parseAliasStr
  ]

parseAliasDef :: Stream s m Char => ParsecT s u m Alias
parseAliasDef = do
  alias <- many1 $ noneOf " "
  spaces
  string "="
  a2 <- many parseAliasElem
  return $ Alias alias a2

parseAllAliasDefs :: Stream s m Char => ParsecT s u m [Alias]
parseAllAliasDefs = many $ do {x <- parseAliasDef; newline; return x} 

applyAlias :: Alias -> String -> Maybe String
applyAlias (Alias alias rewrite) c = do
  args <- stripPrefix alias c
  let argSplit = words args
  return $ applyArgs rewrite argSplit

aliasComplete :: [Alias] -> String -> String
aliasComplete [] s = s
aliasComplete (x:xs) s = case applyAlias x s of
  Just s' -> s'
  Nothing -> aliasComplete xs s

testAliases :: String -> IO String
testAliases s = do
  aliases <- parseAliases2 "testing"
  return $ aliasComplete (fromJust aliases) s

applyArgs :: [AliasElem] -> [String] -> String
applyArgs alias args = concat $ intersperse " " substituted
  where
    substituted = subs args <$> alias

subs :: [String] -> AliasElem -> String
subs _ (AliasStr s) = s
subs args AliasArgWildCard = concat $ intersperse " " args
-- Args are 1-indexed
subs args (AliasArg n) = args !! (n-1)
