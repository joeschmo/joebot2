{-| 
  Core is the internal implementation of joebot2.
  For the most part, only parts of Core are exposed to
  the plugin writer, namely "Core.Types" and "Core.Cmds".
-}
module Joebot.Core (module C) where
import Joebot.Core.Cmds as C
import Joebot.Core.Types as C


