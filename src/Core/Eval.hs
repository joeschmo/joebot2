{-# LANGUAGE OverloadedStrings #-}
module Core.Eval where

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

-- | Takes a response and evaluates it.
eval :: Response -> Net ()
eval (Ping serv) = write "PONG" $ " :"<>serv
eval (Part n ch) = do
    c <- asks config
    mapM_ (($ ch) . ($ n)) (c^.phooks)
eval (Join n ch) = do
    c <- asks config
    mapM_ (($ ch) . ($ n)) (c^.jhooks)
eval (Req req) = do
    cmd <- getCmd (req^.cname)
    case cmd of
      Nothing -> return ()
      Just c  -> evalCmd c (req^.name) (req^.chn) (req^.tokens)
eval _ = return ()

-- | Looks up a command from the configuration
getCmd :: T.Text -> Net (Maybe Command)
getCmd cmd = do
    c <- asks config
    let cs = zip (c^.cmds.to (map $ view cmdName)) (c^.cmds)
    return $ lookup cmd cs

-- | Evaluates a command given name, channel, and arguments
evalCmd :: Command -> T.Text -> Maybe T.Text -> [T.Text] -> Net ()
evalCmd cmd rcp chnl args
  | (length args) < (cmd^.arity) = privmsg rcp chnl (rcp<>": "<>cmd^.help)
  | otherwise                    = (cmd^.runCmd) rcp chnl args

-- | Generic write to socket
write :: T.Text -> T.Text -> Net ()
write s t = do
    h <- asks socket
    liftIO $ BS.hPutStr h (E.encodeUtf8 (s<>" "<>t<>" \r\n")) >> hFlush h
    liftIO $ T.putStrLn ("> "<>s<>" "<>t) >> hFlush stdout

-- | Private messaging
privmsg :: T.Text -> Maybe T.Text -> T.Text -> Net ()
privmsg n (Just ch) s = write "PRIVMSG" $ ch <> " :" <> s
privmsg n Nothing   s = write "PRIVMSG" $ n  <> " :" <> s

-- | Running an action
action :: T.Text -> Net ()
action s = do
    c <- asks config
    write "PRIVMSG" $ (c^.chan) <> " :\001ACTION " <> s


