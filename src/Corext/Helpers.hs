module Corext.Helpers where

import System.Environment
import Data.Maybe

inetroot :: IO (Maybe String)
inetroot = lookup "INETROOT" <$> getEnvironment

inCorext :: IO Bool
inCorext = isJust <$> inetroot