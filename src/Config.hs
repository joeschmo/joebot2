{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Config where

import Network
import System.IO
import Control.Lens
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.Reader

import Data.Monoid

import qualified Data.Text as T

import Core.Types
import Core.Cmds
import Core.Connect

type JHook = T.Text -> T.Text -> Net ()
type PHook = T.Text -> T.Text -> Net ()

defaultConfig = Config "default-bot" "ircbot" "irc.freenode.net" 6667 "#joebot-test" Nothing defaultCmds [] []

defaultCmds = [ echo, poke, slap, spoil, itshere, botsnack, ping
              , commands, usage]

joebot :: Config -> IO ()
joebot conf = bracket (connect conf) disconnect loop
  where disconnect = hClose . socket
        loop st    = runReaderT run st

run :: Net ()
run = do
  c <- asks config
  write "NICK" (c^.nick)
  write "USER" (c^.nick <> " 0 * :" <> c^.rname)
  write "JOIN" (c^.chan)
  maybe (return ()) (\p -> privmsg "NickServ" Nothing p) (c^.pass)
  asks socket >>= listen

spawn :: Command -> Net (Maybe (Chan Msg), Command)
spawn cmd =
  case cmd^.runCmd of
    Left _ -> return (Nothing, cmd)
    Right (_,f) -> do
      ch <- liftIO newChan
      liftIO $ forkIO (f ch)
      return $ (Just ch, cmd & runCmd .~ (Right (ch, f)))
        
withHooks :: Command 
          -> [(Chan Msg -> JHook)]
          -> [(Chan Msg -> PHook)]
          -> Net (Command, [JHook], [PHook])
withHooks cmd j p = do
    (ch, c) <- spawn cmd
    return $ maybe (c, [], [])
                   (\ch' -> ( c
                   , map (\jhook -> jhook ch') j
                   , map (\phook -> phook ch') p
                   ))
                   ch

withJHook cmd jhook = withHooks cmd [jhook] []
withPHook cmd phook = withHooks cmd [] [phook]
