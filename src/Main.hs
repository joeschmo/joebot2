{-# LANGUAGE OverloadedStrings #-}
import Config
import PluginUtils
import Joebot.Plugins.Mail.Base
import Joebot.Plugins.Mail.Cmd
import Joebot.Plugins.Dice.Roll
import Joebot.Plugins.Steam.Core
import Joebot.Plugins.Steam.Cmds

import Control.Concurrent.Chan

import System.Exit
import Control.Lens
import Control.Monad.Reader
import Data.Monoid
import Joebot.Core

main = do
  conf <- spawnProc defaultConfig
                    mailProc 
                    [mail, rcv, inbox]
                    [mHook]
                    []
                    False
  joebot $ conf & cmds %~ ((<>) [roll, quit])

quit = Command "!quit" 0 quit' "!quit"
  where quit' _ _ _ = do
          c <- asks config
          mapM_ (\ch -> 
                  liftIO $ writeChan ch Quit
                  >> readChan ch) 
                (c^.procChans)
          write "QUIT" ":Deactivated"
          liftIO $ exitWith ExitSuccess
