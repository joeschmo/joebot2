{-# LANGUAGE OverloadedStrings #-}
module Plugins.Steam.Cmds where

import Data.Monoid
import Control.Concurrent.Chan
import System.IO

import qualified Data.Map as M
import qualified Data.Text as T
import Core
import Control.Monad.State
import Control.Lens

import Plugins.Steam.Core
import Plugins.Steam.Types
import Plugins.Steam.Util

sreg ch = Command "!sreg" 1 (register ch) "!sreg <steam id> -- Register steam name to list"

slist ch = Command "!slist" 0 (steamlist ch) "!slist -- Show status of people on steam"

register ch n chn args = do
  liftIO $ writeChan ch $ Msg n chn "!sreg" args
  _ <- liftIO $ readChan ch
  privmsg n chn "Registered."

steamlist ch n chn args = do
  liftIO $ writeChan ch $ Msg n chn "!slist" args
  Res statuses <- liftIO $ readChan ch
  mapM_ (privmsg n Nothing) statuses

steamProc :: T.Text -> FilePath -> Chan Msg -> IO () 
steamProc steamkey file ch = do
  usrs <- loadUsers file
  case usrs of
    Just usrs -> do
      (_,u) <- runStateT (userServ ch) $ ServState usrs steamkey
      saveUsers (u^.users) file
      writeChan ch $ Res []
    Nothing   -> do
      (_,u) <- runStateT (userServ ch) $ ServState M.empty steamkey
      saveUsers (u^.users) file
      writeChan ch $ Res []
