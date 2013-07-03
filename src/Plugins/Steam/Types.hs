{-# LANGUAGE TemplateHaskell #-}
module Plugins.Steam.Types where

import qualified Data.Text as T
import qualified Data.Map as M
import Control.Lens
import Control.Monad.State

data User = User
    { _sid    :: T.Text
    , _pname  :: T.Text
    , _pstate :: Int
    , _game   :: Maybe T.Text
    }

makeLenses ''User 

type Users = M.Map T.Text T.Text
data ServState = ServState 
  { _users :: Users
  , _skey :: T.Text
  }

makeLenses ''ServState
type UserServ = StateT ServState IO

