{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Config where

import Network
import System.IO
import Control.Lens
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.Reader

import Data.Monoid

import qualified Data.Text as T

import Core

type JHook = T.Text -> T.Text -> Net ()
type PHook = T.Text -> T.Text -> Net ()

defaultConfig = Config "default-bot" "ircbot" "irc.freenode.net" 6667 "#joebot-test" Nothing defaultCmds [] []

defaultCmds = [ echo, poke, slap, spoil, itshere, botsnack, ping
              , commands, usage, source, version]

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
