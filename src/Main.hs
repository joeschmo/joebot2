{-# LANGUAGE OverloadedStrings #-}
import Config
import PluginUtils
import Plugins.Mail.Base
import Plugins.Mail.Cmd
import Plugins.Dice.Roll

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
                & nick  .~ "joe_bot"
                & rname .~ "resident bot of #roboclub"
                & chan  .~ "#roboclub"
                & pass  .~ Just "cantbotthis"

quit = Command "!quit" 0 quit' "!quit"
  where quit' _ (Just _) _ = return ()
        quit' n Nothing _ =
          if n == "josephle"
          then do
            c <- asks config
            mapM_ (\ch -> 
                    liftIO $ writeChan ch Quit
                    >> readChan ch) 
                  (c^.procChans)
            write "QUIT" ":Deactivated"
            liftIO $ exitWith ExitSuccess
          else return ()
