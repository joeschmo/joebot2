{-# LANGUAGE OverloadedStrings #-}

module Plugins.Steam.Util where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Network.HTTP.Conduit
import Network

import System.IO

import Data.Monoid
import Data.Aeson
import Data.Aeson.Types

import qualified Data.Map as M
import qualified Data.Vector as V
import Control.Monad
import Control.Monad.State

import Plugins.Steam.Types

getUserJson :: T.Text -> T.Text -> IO LBS.ByteString
getUserJson sid apikey = do
  let url = "http://api.steampowered.com/ISteamUser/GetPlayerSummaries/"
         <> "v0002/?key=" <> apikey
         <> "&steamids=" <> sid
  request' <- parseUrl $ T.unpack url
  let request = request' { checkStatus = \ _ _ _ -> Nothing }
  json <- withManager $ \manager -> do
    res <- httpLbs request manager
    return $ responseBody res
  return json

headMaybe [] = Nothing
headMaybe (x:xs) = Just x

parseUserJson :: LBS.ByteString -> Maybe User
parseUserJson json = do
  ast <- decode' json
  usrs <- flip parseMaybe ast $ \obj -> do
    res <- obj .: "response"
    usrs <- res .: "players"
    usrs' <- mapM parseUserStats usrs
    return usrs'
  headMaybe usrs
    
parseUserStats obj = do
  sid <- obj .: "steamid"
  pn  <- obj .: "personaname"
  pst <- obj .: "personastate"
  g   <- obj .:? "gameextrainfo"
  return $ User sid pn pst g

saveUsers :: Users -> FilePath -> IO ()
saveUsers usrs saveLoc = do
  let json = encode usrs
  LBS.writeFile saveLoc json

loadUsers :: FilePath -> IO (Maybe Users)
loadUsers saveLoc = do
  usrs <- LBS.readFile saveLoc
  let json = decode' usrs
  return json
