{-# LANGUAGE OverloadedStrings, TemplateHaskell, ImpredicativeTypes #-}
module Core.Types where

import qualified Data.Text as T
import Control.Lens
import Control.Lens.TH
import Control.Concurrent.Chan
import System.IO
import Network
import Control.Monad.Reader

data Bot = Bot 
    { socket :: Handle 
    , config :: Config
    }

type Net = ReaderT Bot IO

data Msg = Msg T.Text (Maybe T.Text) [T.Text] 
         | Res T.Text
         | Act T.Text
         | Quit

data Command = Command 
    { _cmdName :: T.Text
    , _arity   :: Int
    -- The command given is either a sequential command
    -- or a running thread in the background (i.e. has persistent state)
    , _runCmd  :: Either (T.Text -> Maybe T.Text -> [T.Text] -> Net ())
                         (Chan Msg, Chan Msg -> IO ())
    -- what is shown if the command is invoked incorrectly
    , _help    :: T.Text
    }

data Config = Config 
    { _nick   :: T.Text
    , _rname  :: T.Text
    , _server :: String
    , _port   :: Int
    , _chan   :: T.Text
    , _pass   :: Maybe T.Text
    , _cmds   :: [Command]
    , _jhooks :: [T.Text -> T.Text -> Net ()]
    , _phooks :: [T.Text -> T.Text -> Net ()]
    }

data Response = 
    Req Request
  | Join T.Text T.Text -- nick chan
  | Part T.Text T.Text-- nick chan
  | Ping T.Text
  | Txt T.Text

data Request = Request
    { _name   :: T.Text
    , _chn    :: Maybe T.Text
    , _cname  :: T.Text
    , _tokens :: [T.Text]
    }

makeLenses ''Command
makeLenses ''Config
makeLenses ''Request
