{-# LANGUAGE OverloadedStrings #-}
module Mail.Cmd where

import qualified Data.Text as T
import Control.Concurrent.Chan

import Core
import Mail.Base

mail :: Chan Msg -> Command
mail ch = Command "!mail" 1 (send ch) "!mail <nick> <text>" 

rcv :: Chan Msg -> Command
rcv ch = Command "!rcv" 0 (receive ch) "!rcv"

inbox :: Chan Msg -> Command
inbox ch = Command "!inbox" 0 (checkInbox ch) "!inbox"

mHook :: Chan Msg -> T.Text -> T.Text -> Net ()
mHook ch n chnl = checkInbox ch n Nothing [] 
