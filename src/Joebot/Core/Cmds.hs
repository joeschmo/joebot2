{-# LANGUAGE OverloadedStrings #-}
module Joebot.Core.Cmds where

import Network
import System.IO
import Text.Printf
import Control.Monad.Reader
import Control.Lens
import Control.Concurrent.Chan
import Control.Applicative
import Data.Monoid

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as E
import System.Exit

import Joebot.Core.Types 
import qualified Joebot.Core.Eval as E

-- | Generic write to socket
write :: T.Text -> T.Text -> Net ()
write   = E.write

-- | Used to send PRIVMSG responses to the socket.
--
--  > privmsg nick channel message.
--
--  If channel is @Nothing@, then it will send the message
--  as a private message to nick.
privmsg :: T.Text -> Maybe T.Text -> T.Text -> Net ()
privmsg = E.privmsg

-- | Used to send ACTION responses to the socket.
action :: T.Text -> Net ()
action  = E.action

-- | Used to grab the help messages of a Cmd
getHelp :: T.Text -> Net (Maybe T.Text)
getHelp cmd = (pure $ maybe Nothing (^.help.to Just)) <*> E.getCmd cmd

