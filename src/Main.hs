{-# LANGUAGE OverloadedStrings #-}
import Config
import PluginUtils
import Plugins.Mail.Base
import Plugins.Mail.Cmd
import Plugins.Dice.Roll
import Plugins.Steam.Core
import Plugins.Steam.Cmds

import Control.Concurrent.Chan

import System.Exit
import Control.Lens
import Control.Monad.Reader
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
          c <- asks config
          mapM_ (\ch -> 
                  liftIO $ writeChan ch Quit
                  >> readChan ch) 
                (c^.procChans)
          write "QUIT" ":Deactivated"
          liftIO $ exitWith ExitSuccess
