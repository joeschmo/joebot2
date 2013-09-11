{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Config ( defaultConfig
              , joebot ) where

import Network
import System.IO
import Control.Lens
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.Reader

import Data.Monoid

import qualified Data.Text as T

import Joebot.Core
import Joebot.Core.Connect
import Joebot.Plugins.Prelude

-- | Default configuration for joebot2
--
--      [@nick@] default-bot
--
--      [@rname@] ircbot
--
--      [@server@] irc.freenode.net
--
--      [@port@] 6667
--
--      [@channel@] #joebot-test
--
--      [@cmds@] see 'Cmds' for a list of all commands in the default configuration
--
defaultConfig = Config "default-bot" "ircbot" "irc.freenode.net" 6667 "#joebot-test" Nothing defaultCmds [] [] [] []

defaultCmds = [ echo, poke, slap, spoil, itshere, botsnack, ping
              , commands, usage, source, version ]

-- | Takes a 'Config' and runs joebot2
joebot :: Config -> IO ()
joebot conf = bracket (connect conf) disconnect loop
  where disconnect = hClose . socket
        loop st    = runReaderT run st

run :: Net ()
run = do
  c <- asks config
  write "NICK" (c^.nick)
  write "USER" (c^.nick <> " 0 * :" <> c^.rname)
  write "JOIN" (c^.chan)
  maybe (return ()) 
        (\p -> privmsg "NickServ" Nothing $ "identify "<>(c^.nick)<>" "<>p) 
        (c^.pass)
  asks socket >>= listen
