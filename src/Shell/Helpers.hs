module Shell.Helpers where

import Shell.Types

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

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

-- f Only if x
infix 0 ?->
(?->) :: (Applicative f) => Bool -> f() -> f ()
(?->) = flip (<-?)

-- Run some arbitrary IO if we are running in debug mode
ifDebug :: IO () -> StateT FishyState IO ()
ifDebug f = lift f <~? getDebug <$> get
