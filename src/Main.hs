import Config
import PluginUtils
import Mail.Base
import Mail.Cmd
import Dice.Base

import System.Exit
import Control.Lens
import Control.Monad.Trans
import Data.Monoid
import Core

main = do
  conf <- spawnProc defaultConfig
                    mailProc 
                    [mail, rcv, inbox]
                    [mHook]
                    []
  joebot $ conf & cmds %~ ((<>) [roll, quit])

quit = Command "!quit" 0 quit' "!quit"
  where quit' _ _ _ = do
          write "QUIT" ":Deactivated"
          liftIO $ exitWith ExitSuccess
