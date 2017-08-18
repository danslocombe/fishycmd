module Corext.AliasCompleter where

import Safe
import Data.Maybe
import Data.List.Split
import System.Environment

inetroot :: IO (Maybe String)
inetroot = lookup "%INETROOT%" <$> getEnvironment

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
      aliasKeys =  mapMaybe (headMay . (splitOn " ")) aliasKeysWhitespace
  return aliasKeys
