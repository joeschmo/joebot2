{-# LANGUAGE OverloadedStrings #-}
module Core.Cmds where

import Network
import System.IO
import Text.Printf
import Control.Monad.Reader
import Control.Lens
import Control.Concurrent.Chan
import Data.Monoid

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as E
import System.Exit

import Core.Types 
import qualified Core.Eval as E

write   = E.write
privmsg = E.privmsg
action  = E.action

-- Some default commands
echo = Command "!echo" 0 (\n ch args -> privmsg n ch (T.unwords args))
               "!echo <text>"

poke = Command "!poke" 1 (\n ch args -> action $ "prods "<>(head args))
               "!poke <nick>"

slap = Command "!slap" 1 (\n ch args -> action $
                            "grabs "<>(head args)<>" and slaps them silly")
               "!slap <nick>"

spoil = Command "!spoil" 1 spoil' "!spoil <text>"
  where spoil' n ch args = privmsg n ch $ "in "<>(head args)<>", you're waifu dies"

itshere = Command "!itshere" 0 itshere' "!itshere"
  where itshere' n ch _ = privmsg n ch $ "キターーーーーーーーーーー！！！"

botsnack = Command "!botsnack" 0 snack "!botsnack"
  where snack n ch _ = privmsg n ch $ ":)"

ping = Command "!ping" 1 ping' "!ping <nick>"
  where ping' n ch args = privmsg n ch $ (head args)<>": you there?"

commands = Command "!cmds" 0 commands' "!cmds"
  where commands' n ch args = do
          c <- asks config
          privmsg n ch $ T.unwords (c^.cmds.to (map $ view cmdName))

usage = Command "!help" 1 usage' "!help <command>"
  where usage' n ch args = do
          cmd <- E.getCmd (head args)
          case cmd of
            Nothing -> privmsg n ch $ 
                        "no such command, use !cmd to list commands"
            Just c  -> privmsg n ch $ (c^.help)

source = Command "!source" 0 source' "!source"
  where source' n ch args = privmsg n ch $ 
            "Source code for joebot2 is found at: https://github.com/joeschmo/joebot2"

version = Command "!version" 0 version' "!version"
  where version' n ch args = privmsg n ch $ "joebot2 version 1.0"
