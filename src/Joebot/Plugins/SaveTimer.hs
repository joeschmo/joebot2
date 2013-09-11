{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-}

-- An update timer plugin that can be used
-- for setting up plugins that should be saved
-- periodically.
module Joebot.Plugins.UpdateTimer where

import Control.Wire
import Control.Lens
import Control.Concurrent
import Control.Concurrent.MVar

import Data.Monoid
import Control.Monad.Identity

period :: Time -> Wire () Identity () ()
period = periodically

timerLoop w' session' ch = do
  (mx, w, session) <- stepSessionP w' session' ()
  case mx of
    Left _  -> timerLoop w session ch
    Right _ -> periodSignal ch >> timerLoop w session ch 

runTimer ch w = timerLoop w clockSession ch

periodSignal :: Chan () -> IO ()
periodSignal ch = writeChan ch ()
