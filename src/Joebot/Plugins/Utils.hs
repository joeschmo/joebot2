{-# LANGUAGE OverloadedStrings #-}
module Joebot.Plugins.Utils where

import Control.Concurrent.Chan
import Control.Concurrent
import Control.Lens
import Data.Monoid

import qualified Data.Text as T
import Joebot.Core.Types

-- |The type for a hook (Join and Part)
type Hook = T.Text -> T.Text -> Net ()
-- |The type for a Plugin Process
type Proc = Chan Msg -> IO ()

-- |Spawn a process given lists of
-- commands and hooks. Returns a new
-- configuration containing the commands
-- and hooks.
spawnProc :: Config 
          -> Proc 
          -> [Chan Msg -> Command] 
          -> [Chan Msg -> Hook] 
          -> [Chan Msg -> Hook] 
          -> Bool
          -> IO Config
spawnProc conf proc coms js ps quit = do
    ch <- newChan
    forkIO $ proc ch
    return $ updateConfig conf ch coms js ps quit 

-- |Updates a configuration with a series of
-- commands and hooks.
updateConfig :: Config
             -> Chan Msg
             -> [Chan Msg -> Command]
             -> [Chan Msg -> Hook]
             -> [Chan Msg -> Hook]
             -> Bool
             -> Config
updateConfig conf ch coms js ps quit =
    let 
      procCh = if quit then [ch] else []
    in
      conf & cmds %~ ((<>) (map ($ ch) coms))
           & jhooks %~ ((<>) (map ($ ch) js))
           & phooks %~ ((<>) (map ($ ch) ps))
           & procChans %~ ((<>) procCh)
