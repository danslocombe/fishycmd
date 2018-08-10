module CLI.Modes (lookupMode) where

import CLI.Types
import CLI.ShellMode
import CLI.SearchMode

-- To avoid circular dependencies

lookupMode :: CLIMode -> Mode
lookupMode ShellMode = shellMode
lookupMode SearchMode = searchMode

shellMode :: Mode
shellMode = Mode
  { update = shellUpdate
  , draw = shellDraw
  }

searchMode :: Mode
searchMode = Mode
  { update = searchUpdate
  , draw = searchDraw
  }
