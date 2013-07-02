{-# LANGUAGE OverloadedStrings, Rank2Types, ImpredicativeTypes #-}
module Core.Cmds where

import Network
import System.IO
import Text.Printf
import Control.Monad.Reader
import Control.Lens
import Control.Concurrent.Chan
import Data.Monoid

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as E
import System.Exit

import Core.Types 

eval :: Response -> Net ()
eval (Ping serv) = write "PONG" $ " :"<>serv
eval (Part n ch) = do
    c <- asks config
    mapM_ ($ ch) $ map ($ n) (c^.phooks)
eval (Join n ch) = do
    c <- asks config
    mapM_ ($ ch) $ map ($ n) (c^.jhooks)
eval (Req req) = do
    cmd <- getCmd (req^.cname)
    case cmd of
      Nothing -> return ()
      Just c  -> evalCmd c (req^.name) (req^.chn) (req^.tokens)
eval _ = return ()

getCmd :: T.Text -> Net (Maybe Command)
getCmd cmd = do
    c <- asks config
    let cs = zip (c^.cmds.to (map $ view cmdName)) (c^.cmds)
    return $ lookup cmd cs


-- evaluate a command given name, channel, and arguments
evalCmd :: Command -> T.Text -> Maybe T.Text -> [T.Text] -> Net ()
evalCmd cmd rcp chnl args
  | (length args) < (cmd^.arity) = do
        c <- asks config
        privmsg rcp chnl (rcp<>": "<>cmd^.help)
  | otherwise = (cmd^.runCmd) rcp chnl args

-- Generic write to socket
write :: T.Text -> T.Text -> Net ()
write s t = do
    h <- asks socket
    liftIO $ BS.hPutStr h (E.encodeUtf8 (s<>" "<>t<>" \r\n")) >> hFlush h
    liftIO $ T.putStrLn ("> "<>s<>t) >> hFlush stdout

-- Private messaging
privmsg :: T.Text -> Maybe T.Text -> T.Text -> Net ()
privmsg n (Just ch) s = write "PRIVMSG" $ ch <> " :" <> s
privmsg n Nothing   s = write "PRIVMSG" $ n  <> " :" <> s

-- Running an action
action :: T.Text -> Net ()
action s = do
    c <- asks config
    write "PRIVMSG" $ (c^.chan) <> " :\001ACTION " <> s

-- Some default commands
echo = Command "!echo" 0 (\n ch args -> privmsg n ch (T.unwords args))
               "!echo <text>"

poke = Command "!poke" 1 (\n ch args -> action $ "prods "<>(head args))
               "!poke <nick>"

slap = Command "!slap" 1 (\n ch args -> action $
                            "grabs "<>(head args)<>" and slaps them silly")
               "!slap <nick>"

spoil = Command "!spoil" 1 spoil' "!spoil <text>"
  where spoil' n ch args = privmsg n ch $ "in "<>(head args)<>", you're waifu dies"

itshere = Command "!itshere" 0 itshere' "!itshere"
  where itshere' n ch _ = privmsg n ch $ "キターーーーーーーーーーー！！！"

botsnack = Command "!botsnack" 0 snack "!botsnack"
  where snack n ch _ = privmsg n ch $ ":)"

ping = Command "!ping" 1 ping' "!ping <nick>"
  where ping' n ch args = privmsg n ch $ (head args)<>": you there?"

commands = Command "!cmds" 0 commands' "!cmds"
  where commands' n ch args = do
          c <- asks config
          privmsg n ch $ T.unwords (c^.cmds.to (map $ view cmdName))

usage = Command "!help" 1 usage' "!help <command>"
  where usage' n ch args = do
          cmd <- getCmd (head args)
          case cmd of
            Nothing -> privmsg n ch $ 
                        "no such command, use !cmd to list commands"
            Just c  -> privmsg n ch $ (c^.help)

source = Command "!source" 0 source' "!source"
  where source' n ch args = privmsg n ch $ 
            "Source code for joebot2 is found at: https://github.com/joeschmo/joebot2"

version = Command "!version" 0 version' "!version"
  where version' n ch args = privmsg n ch $ "joebot2 version 1.0"
