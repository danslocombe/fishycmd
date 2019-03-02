{-# LANGUAGE ForeignFunctionInterface #-}

module CLI.KeyPress
  ( matchChar
  , getHiddenChar
  ) where

import Foreign.C.Types
import GHC.IO.Exception
import Data.Char (chr, ord)
import CLI.State
import CLI.Types
import CLI.ShellMode.Prompt

import Data.List.Zipper hiding (insert)

getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt

-- Hardcoded windows inputs
matchChar :: FishyState -> Char -> CommandInput
matchChar state c = case ord c of
  10  -> Run                          -- Newline
  13  -> Run                          -- Newline (Windows)
  8   -> Text $ Zip (drop 1 s) s'     -- Backspace (Windows)
  127 -> Text $ removeBlockLeft p            -- Backspace (Windows Ctr+backspace)
  6   -> Complete                     -- Complete (Windows Ctr+F form feed)
  9   -> PartialComplete              -- Partial complete (Tab)
  12  -> Cls                          -- Clear screen (Ctr+L)
  3   -> Exit                         -- Exit (Windows Ctr+C)
  4   -> Exit                         -- EOF (Windows Ctr+D)
  224 -> PrepControlChar              -- Prep character
  75  -> ifControlPrepped $ left p    -- Left if prepped
  77  -> ifControlPrepped' $          -- Right if prepped
         if s' == [] then   -- If we are all the way to the right then complete
            PartialComplete
            else Text $ right p  -- Otherwise move cursor
  115 -> ifControlPrepped $ moveBlockLeft p    -- Ctrl+Left if prepped
  116 -> ifControlPrepped $ moveBlockRight p   -- Ctrl+Right if prepped
  14  -> HistoryForward               -- Ctrl p
  16  -> HistoryBack                  -- Ctrl n
  18  -> Search                       -- Ctrl r
  72  -> if getControlPrepped state then HistoryBack else Text (push c p)
  80  -> if getControlPrepped state then HistoryForward else Text (push c p)
  71  -> ifControlPrepped $ Zip [] (toList p) -- Home
  79  -> ifControlPrepped $ Zip (reverse $ toList p) [] -- End
  x   -> Text $ push c p
  where p@(Zip s s') = getPrompt state
        ifControlPrepped r = Text $
          if getControlPrepped state then r else push c p
        ifControlPrepped' r = 
          if getControlPrepped state then r else Text $ push c p

