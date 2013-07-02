{-# LANGUAGE OverloadedStrings #-}
module Mail.Base (mailProc, send, receive, checkInbox) where

import qualified Data.Text as T
import Control.Concurrent.Chan
import Control.Monad.State
import Control.Arrow
import Control.Lens
import Data.Text.Lens

import Data.Monoid
import qualified Data.Map as M
import qualified Data.Foldable as F

import Core

type Mailbox = M.Map T.Text [(T.Text, T.Text)]
type MailServ = StateT Mailbox IO

mailProc :: Chan Msg -> IO ()
mailProc ch = runStateT (mailbox ch) M.empty >> return ()

mailbox :: Chan Msg -> MailServ ()
mailbox ch = do
    msg <- liftIO $ readChan ch
    txts <- evalMsg msg
    liftIO $ writeChan ch (Res txts) 
    mailbox ch

evalMsg :: Msg -> MailServ [T.Text]
evalMsg (Msg n _ "!inbox" _) = do
    mbox <- get
    case mbox^.at n of
      Nothing -> return ["Your inbox is empty"]
      Just ms ->
        if null ms then return ["Your inbox is empty"]
        else return ["You have "<>(ms^.to length^.to show^. packed)<>
                     " msg(s). "<>"Use !rcv to read."]
evalMsg (Msg n _ "!rcv" _) = do
    mbox <- get
    case mbox^.at n of
      Nothing -> return ["Your inbox is empty"]
      Just ms -> do
            put (at n ?~ [] $ mbox)
            return $
                F.foldl' 
                    (\msgs (sndr, msg) ->
                        ("From "<>sndr<>":") : msg : msgs)
                    []
                    ms
evalMsg (Msg n _ "!mail" txt) = do
    mbox <- get
    if length txt < 1 
        then return ["!mail <nick> <text>"]
        else do
            let rcp = head txt
            let msg  = T.unwords $ tail txt
            case mbox^.at n of
              Nothing -> do
                put (at rcp ?~ [(n, msg)] $ mbox)
              Just ms -> do
                put (at rcp ?~ ((n, msg) : ms) $ mbox)
            return ["Sent."]
evalMsg (Msg n _ _ txt) = return []

queryBox :: Chan Msg -> T.Text -> Maybe T.Text 
         -> T.Text -> [T.Text] -> T.Text -> Net ()
queryBox ch n chn cmd txt help = do
    liftIO $ writeChan ch $ Msg n chn cmd txt
    (Res msgs) <- liftIO $ readChan ch
    if null msgs 
        then (privmsg n Nothing $ "usage: "<>help)
        else mapM_ (privmsg n Nothing) msgs

send ch n chn txt = do
    liftIO $ writeChan ch $ Msg n chn "!mail" txt
    (Res msgs) <- liftIO $ readChan ch
    mapM_ (privmsg n Nothing) msgs
    privmsg (head txt) Nothing "You have new mail. Use !rcv to read."

receive ch n chn txt = queryBox ch n chn "!rcv" [] "Your inbox is empty."
checkInbox ch n chn txt = queryBox ch n chn "!inbox" [] "!inbox"
