{-# LANGUAGE OverloadedStrings #-}
module Joebot.Plugins.Mail.Cmd where

import qualified Data.Text as T
import Control.Concurrent.Chan

import Joebot.Core
import Joebot.Plugins.Mail.Base

mail :: Chan Msg -> Command
mail ch = Command "!mail" 1 (send ch) "!mail <nick> <text> -- send a message" 

rcv :: Chan Msg -> Command
rcv ch = Command "!rcv" 0 (receive ch) "!rcv -- read all messages"

inbox :: Chan Msg -> Command
inbox ch = Command "!inbox" 0 (checkInbox ch) "!inbox -- check how many messages you have"

mHook :: Chan Msg -> T.Text -> T.Text -> Net ()
mHook ch n chnl = checkInbox ch n Nothing [] 
