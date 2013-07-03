{-# LANGUAGE OverloadedStrings #-}
module Plugins.Steam.Cmds where

import Data.Monoid
import Control.Concurrent.Chan
import System.IO

import qualified Data.Map as M
import qualified Data.Text as T
import Core

import Plugins.Steam.Core
import Plugins.Steam.Types

sreg ch = Command "!sreg" 2 (register ch) "!sreg <nick> <steam id> -- Register steam name to list"

slist ch = Command "!slist" 0 (steamlist ch) "!list -- Show status of people on steam"

steamProc :: T.Text -> Chan Msg -> IO () 
steamProc steamkey ch = do
  usrs <- loadUsers
  (_,u) <- runStateT (userServ ch) $ ServState usrs steamkey
  saveUsers u
