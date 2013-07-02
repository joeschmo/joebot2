{-# LANGUAGE OverloadedStrings #-}
module Mail.Base where

import qualified Data.Text as T
import Control.Concurrent.Chan
import Control.Monad.State
import Control.Lens
import Control.Lens.Map
import Control.Lens.Text

import qualified Data.Map as M

type Mailbox = M.Map T.Text [(T.Text, T.Text)]
type MailServ = StateT Mailbox IO


