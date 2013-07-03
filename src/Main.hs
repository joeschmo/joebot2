import Config
import PluginUtils
import Plugins.Mail.Base
import Plugins.Mail.Cmd
import Plugins.Dice.Base

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
