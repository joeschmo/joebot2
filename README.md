joebot2
=======

Fully Customizable Haskell IRC Bot

#Introduction
joebot2 is an upgrade from the original joe\_bot that runs irc.freenode.net #roboclub.
joebot2 was designed to be an easy to customize and upgrade haskell irc bot.

#Installation
To install joe\_bot, you need the latest [haskell platform](http://www.haskell.org).
Afterwards, just run <code>cabal install</code> in the directory containing
<code>joebot.cabal</code>.

To run after installing, just run <code>joe\_bot</code>

#The Basics
The bare minimum for <code>Main.hs</code> is:

    import Config

    main = joebot $ defaultConfig
This will run joebot2 with the default configurations specified in <code>Config.hs</code>

Changing the configuration is relatively simple:

    {-# LANGUAGE OverloadedStrings #-}
    import Config

    main = joebot $ defaultConfig
        { _nick = "test_bot"
        , _chan = "#haskell"
        , _pass = Just "fake_password"
        }

More advanced users will note that joebot2 uses the 
[Control.Lens](https://github.com/ekmett/lens#lens-lenses-folds-and-traversals)
library extensively. The above code snippet can be then rewritten to use lenses:

    {-# LANGUAGE OverloadedStrings #-}
    import Config
    import Control.Lens
    import Core

    main = joebot $ defaultConfig 
        & nick .~ "test_bot"
        & chan .~ "#haskell"
        & pass .~ Just "fake_password"
Whether doing so is overkill is an exercise left to the reader.

The Config type has the following fields:
   
    data Config = Config        -- Field Explanation      Default Value
        { nick   :: Text        -- nickname of bot        "default-bot"
        , rname  :: Text        -- real name of bot       "ircbot" 
        , server :: Text        -- irc server hostname    "irc.freenode.net"
        , port   :: Int         -- port                   6667
        , chan   :: Text        -- irc channel            "#joebot-test"
        , pass   :: Maybe Text  -- password for NickServ  Nothing
        }
There are 3 more fields, but these are the fields should be
changed to run joe\_bot as something other than "default-bot".

#Rolling Your Own Plugins
joebot2 was designed to make it easy to add plugins. Custom plugins are separated into
two concepts: custom commands and plugin processes.

##Custom Commands
A <code>Command</code> is roughly the type

    data Command = Command
        { cmdName :: Text
        , arity   :: Int
        , runCmd  :: Text -> Maybe Text -> [Text] -> Net ()
        , help    :: Text
        }
Here is a brief description of each of the fields:
- <code>cmdName</code> is the name of the command (e.g. "!quit")
- <code>arity</code> is the number of arguments you expect the command to take
- <code>runCmd</code> takes in a nick, a channel, and arguments and executes the command
- <code>help</code> this is what is shown when the command is used incorrectly
