{-# LANGUAGE OverloadedStrings #-}
import Config
import Joebot.Plugins.Utils
import Joebot.Plugins.Mail.Cmds
import Joebot.Plugins.Dice.Roll
import Joebot.Plugins.Steam.Cmds

import Control.Concurrent.Chan

import System.Console.GetOpt
import System.Environment
import Test.QuickCheck

import System.Exit
import Control.Lens
import Control.Monad.Reader
import Data.Monoid
import Joebot.Core
import Joebot.Core.Tests

main = do
  argv <- getArgs
  checkOpts argv $ do
    conf <- spawnProc defaultConfig
                      runMailServer
                      [mail, rcv, inbox]
                      [mHook]
                      []
                      False
    joebot $ conf
        & cmds %~ (<>) [roll, quit]
        & nick .~ "joe_bot"
        & chan .~ "#roboclub"
        & pass .~ Just "cantbotthis"

data Flag = Testing

checkOpts argv action =
  case getOpt Permute options argv of
       (o,n,[]) -> runOpts o action
       (_,_,err) -> action

runOpts (Testing : rem) action = do
  quickCheck prop_joinparse
  quickCheck prop_partparse
  quickCheck prop_privmsgparse
runOpts [] action = action


options :: [OptDescr Flag]
options =
  [ Option [] ["testing"] (NoArg Testing) "Run joebot test suite" ]

quit = Command "!quit" 0 quit' "!quit"
  where quit' _ _ _ = do
          c <- asks config
          mapM_ (\ch ->
                  liftIO $ writeChan ch Quit
                  >> readChan ch)
                (c^.procChans)
          write "QUIT" ":Deactivated"
          liftIO exitSuccess
