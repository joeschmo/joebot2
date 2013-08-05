{-# LANGUAGE OverloadedStrings, TemplateHaskell, ImpredicativeTypes #-}
module Joebot.Core.Types where

import qualified Data.Text as T
import Control.Lens
import Control.Lens.TH
import Control.Concurrent.Chan
import System.IO
import Network
import Control.Monad.Reader

-- |The Bot record type
data Bot = Bot 
    { -- | The socket that joebot should write to
      socket :: Handle 
      -- | The configuration set at startup
    , config :: Config
    }

-- |The Net monad
type Net = ReaderT Bot IO

-- |This is the message type for Plugin Processes 
data Msg 
    -- | Msg name channel cmdName args
    = Msg T.Text (Maybe T.Text) T.Text [T.Text]
    -- | Res txts
    | Res [T.Text]
    -- | Act action_txt
    | Act T.Text
    -- | Used for quitting applications
    | Quit
    deriving Eq

data Command = Command 
    { -- | The name of the command
      _cmdName :: T.Text
      -- | The arity of the command
    , _arity   :: Int
      -- | The command given is either a sequential command
      -- | or a running thread in the background (i.e. has persistent state)
    , _runCmd  :: T.Text -> Maybe T.Text -> [T.Text] -> Net ()
      -- | what is shown if the command is invoked incorrectly
    , _help    :: T.Text
    }

data Config = Config 
    { -- | Nickname for the bot
      _nick   :: T.Text
      -- | Real name of the bot
    , _rname  :: T.Text
      -- | Hostname of the IRC server the bot should connect to.
    , _server :: String
      -- | Connection port. Defaults to 6667
    , _port   :: Int
      -- | Channel the bot should join
    , _chan   :: T.Text
      -- | Password for NickServ if bot is on irc.freenode.net
    , _pass   :: Maybe T.Text
      -- | All the commands the bot can invoke
    , _cmds   :: [Command]
      -- | Catch all for commands that do not conform to
      --   the command spec, i.e. Text hooks
    , _thooks :: [T.Text -> T.Text -> Net ()]
      -- | Join hooks
    , _jhooks :: [T.Text -> T.Text -> Net ()]
      -- | Part/Quit hooks
    , _phooks :: [T.Text -> T.Text -> Net ()]
      -- | List of Channels Plugin Processes are listening on
    , _procChans :: [Chan Msg] 
    }

data Response = 
    Req Request
  | Join T.Text T.Text -- nick chan
  | Part T.Text T.Text -- nick chan
  | Ping T.Text
  | Txt T.Text
  deriving Eq

data Request = Request
    { _name   :: T.Text
    , _chn    :: Maybe T.Text
    , _cname  :: T.Text
    , _tokens :: [T.Text]
    }
    deriving Eq

makeLenses ''Command
makeLenses ''Config
makeLenses ''Request
