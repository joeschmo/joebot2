{-# LANGUAGE OverloadedStrings #-}
module Joebot.Plugins.Steam.Core where

import Data.Monoid
import Data.Either
import Control.Concurrent.Chan
import Control.Monad.State
import Control.Lens
import System.IO

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Joebot.Core

import Joebot.Plugins.Steam.Types
import Joebot.Plugins.Steam.Util

userServ :: Chan Msg -> UserServ ()
userServ ch =
  (liftIO $ readChan ch) >>= (\msg ->
  unless (msg == Quit) $ return msg >>=
    evalMsg >>=
    (liftIO . writeChan ch . Res) >>
    userServ ch) 

evalMsg :: Msg -> UserServ [T.Text]
evalMsg (Msg n _ "!sreg" (sid:args)) = registerNick n sid >> return []
evalMsg (Msg n _ "!slist" _) = nickList >>= processIds
evalMsg (Msg n _ _ _) = return []

registerNick :: T.Text -> T.Text -> UserServ ()
registerNick n sid = modify (& users %~ at n ?~ sid)

nickList :: UserServ [Either T.Text (T.Text, T.Text)]
nickList = gets _users >>= sequence . map getNick . M.keys

getNick :: T.Text -> UserServ (Either T.Text (T.Text, T.Text))
getNick n = do
  ids <- gets _users
  case ids^.at n of
    Nothing  -> return $ Left $ n <> " not found"
    Just sid -> return $ Right (n, sid)

processIds :: [Either T.Text (T.Text, T.Text)] -> UserServ [T.Text]
processIds usrs =
  let
    badNicks = lefts usrs
    ids = rights usrs
  in
    mapM (uncurry processId) ids >>= return . (<> badNicks)

processId :: T.Text -> T.Text -> UserServ T.Text
processId n sid = do
  usrjson <- gets _skey >>= liftIO . getUserJson sid
  let usr = parseUserJson usrjson
  case usr of
    Nothing -> return $ n<>" not found on steam"
    Just usr -> return $ processUsr n usr

processUsr :: T.Text -> User -> T.Text
processUsr n usr =
  case usr^.game of
    Nothing -> n<>" is "<>(getPstate $ usr^.pstate)
    Just gn -> n<>" is playing "<>gn

getPstate :: Int -> T.Text
getPstate 0 = "offline"
getPstate 1 = "online"
getPstate 2 = "busy"
getPstate 3 = "away"
getPstate 4 = "snoozing"
getPstate 5 = "looking to trade"
getPstate 6 = "looking to play"
getPstate _ = "not available"
