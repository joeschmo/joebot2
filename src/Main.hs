{-# LANGUAGE OverloadedStrings #-}
import Config
import Joebot.Plugins.Utils
import Joebot.Plugins.Mail.Cmds
import Joebot.Plugins.Dice.Roll
import Joebot.Plugins.Steam.Cmds

import Control.Concurrent.Chan

import qualified Options.Applicative as OA
import System.Environment
import Test.QuickCheck

import System.Exit
import Control.Lens
import Control.Monad.Reader
import Control.Applicative
import Data.Monoid
import Joebot.Core
import Joebot.Core.Tests

main = do
  fs <- OA.execParser opts
  checkOpts fs
  where
    opts = OA.info (OA.helper <*> flags)
      ( OA.fullDesc
     <> OA.progDesc "Joebot2"
     <> OA.header "joe_bot - a IRC bot written in haskell")

checkOpts fs = do
  when (fs^.to testing) runTests
  conf <- spawnProc defaultConfig
                    runMailServer
                    [mail, rcv, inbox]
                    [mHook]
                    []
                    False
  joebot $ conf
    & cmds %~ (<>) [roll, quit]
    & debugMode .~ (fs^.to debug)


data Flags = Flags
  { testing :: Bool
  , debug   :: Bool
  }

flags :: OA.Parser Flags
flags = Flags
  <$> OA.switch
      ( OA.long "testing"
     <> OA.help "Run joebot test suite")
  <*> OA.switch
      ( OA.long "debug"
     <> OA.help "Run joebot in with debugging output")

runTests = do
  quickCheck prop_joinparse
  quickCheck prop_partparse
  quickCheck prop_privmsgparse

quit = Command "!quit" 0 quit' "!quit"
  where quit' _ _ _ = do
          c <- asks config
          mapM_ (\ch ->
                  liftIO $ writeChan ch Quit
                  >> readChan ch)
                (c^.procChans)
          write "QUIT" ":Deactivated"
          liftIO exitSuccess
