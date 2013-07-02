import Config
import PluginUtils
import Mail.Base
import Mail.Cmd

main = do
  conf <- spawnProc defaultConfig
                    mailProc 
                    [mail, rcv, inbox]
                    [mHook]
                    []
  joebot $ conf
