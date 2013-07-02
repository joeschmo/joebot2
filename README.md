joebot2
=======

Fully Customizable Haskell IRC Bot

#Introduction
joebot2 is an upgrade from the original joe\_bot that runs irc.freenode.net #roboclub.
joebot2 was designed to be an easy to customize and upgrade haskell irc bot.

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
