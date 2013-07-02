{-# LANGUAGE OverloadedStrings #-}
module PluginUtils where

import Control.Concurrent.Chan
import Control.Concurrent
import Control.Lens
import Data.Monoid

import qualified Data.Text as T
import Core

type Hook = T.Text -> T.Text -> Net ()
type Proc = Chan Msg -> IO ()

spawnProc :: Config 
          -> Proc 
          -> [Chan Msg -> Command] 
          -> [Chan Msg -> Hook] 
          -> [Chan Msg -> Hook] 
          -> IO Config
spawnProc conf proc coms js ps = do
    ch <- newChan
    forkIO $ proc ch
    return $ updateConfig conf ch coms js ps 

updateConfig :: Config
             -> Chan Msg
             -> [Chan Msg -> Command]
             -> [Chan Msg -> Hook]
             -> [Chan Msg -> Hook]
             -> Config
updateConfig conf ch coms js ps =
    conf & cmds %~ ((<>) (map ($ ch) coms))
         & jhooks %~ ((<>) (map ($ ch) js))
         & phooks %~ ((<>) (map ($ ch) ps))
