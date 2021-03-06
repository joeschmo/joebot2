joebot2
=======

Fully Customizable Haskell IRC Bot

##Table of Contents
+ [Introduction](#introduction)
+ [Installation](#installation)
+ [The Basics](#the-basics)
+ [Rolling Your Own Plugins](#rolling-your-own-plugins)
    + [Custom Commands](#custom-commands)
    + [Plugin Processes](#plugin-processes)
+ [Examples](#examples)

#Introduction
joebot2 is an upgrade from the original joe\_bot that runs on irc.freenode.net #roboclub.
joebot2 was designed to be an easy to customize and upgrade haskell irc bot.

#Installation
To install joe\_bot, you need the latest [haskell platform](http://www.haskell.org).
Afterwards, just run <code>cabal install</code> in the directory containing
<code>joebot.cabal</code>.

To run after installing, just run <code>joe\_bot</code>.

#The Basics
The bare minimum for <code>Main.hs</code> is:
```haskell
import Config
main = joebot defaultConfig
```
This will run joebot2 with the default configurations specified in [<code>Config.hs</code>](src/Config.hs).

Changing the configuration is relatively simple (note the underscore before each field name):
```haskell
{-# LANGUAGE OverloadedStrings #-}
import Config
main = joebot $ defaultConfig
    { _nick = "test_bot"
    , _chan = "#haskell"
    , _pass = Just "fake_password"
    }
```

More advanced users will note that joebot2 uses the 
[Control.Lens](https://github.com/ekmett/lens#lens-lenses-folds-and-traversals)
library extensively. The above code snippet can be then rewritten to use lenses
(you must import <code>Core</code> to access the lenses):
```haskell
{-# LANGUAGE OverloadedStrings #-}
import Config
import Control.Lens
import Core
main = joebot $ defaultConfig 
    & nick .~ "test_bot"
    & chan .~ "#haskell"
    & pass .~ Just "fake_password"
```
Whether doing so is overkill is an exercise left to the reader.

The Config type has the following fields:
```haskell
data Config = Config        -- Field Explanation      Default Value
    { nick   :: Text        -- nickname of bot        "default-bot"
    , rname  :: Text        -- real name of bot       "ircbot" 
    , server :: Text        -- irc server hostname    "irc.freenode.net"
    , port   :: Int         -- port                   6667
    , chan   :: Text        -- irc channel            "#joebot-test"
    , pass   :: Maybe Text  -- password for NickServ  Nothing
    }
```
There are 3 more fields, but these are the fields that should be
changed to run joe\_bot as something other than "default-bot".

#Rolling Your Own Plugins
joebot2 was designed to make it easy to add plugins. Custom plugins are separated into
two concepts: custom commands and plugin processes.

*Note:* it is recommended that you add the following line
```haskell
{-# LANGUAGE OverloadedStrings #-}
```
to the top of every .hs file. Furthermore, it is recommended that you
import [Data.Text](http://hackage.haskell.org/packages/archive/text/0.11.3.1/doc/html/Data-Text.html) 
like so:
```haskell
import qualified Data.Text as T
```
This is due to the fact that many of the functions provided for plugin support
rely heavily on Data.Text for performance and compatibility purposes.

##Custom Commands
A <code>Command</code> is roughly the type
```haskell
data Command = Command
    { cmdName :: Text
    , arity   :: Int
    , runCmd  :: Text -> Maybe Text -> [Text] -> Net ()
    , help    :: Text
    }
```
Here is a brief description of each of the fields:
- <code>cmdName</code> is the name of the command (e.g. "!quit").
- <code>arity</code> is the number of arguments you expect the command to take.
- <code>runCmd</code> takes in a nick, a channel, and arguments and executes the command.
- <code>help</code> is shown when the command is used incorrectly.

###Example
A simple example of a custom command is found in the 
[Dice Plugin](http://joeschmo.github.io/joebot2/Plugins/Roll.html):
```haskell
roll = Command "!roll" 1 rollDie "!roll <num_dice>d<num_sides>"
```
Here, we see that <code>Command</code> is the constructor, and it takes in the
following arguments:
- <code>"!roll"</code> is the name of the command.
- <code>1</code> roll expects 1 argument, namely an argument of the form "#d#".
- <code>rollDie</code> is the function that is invoked when the command is run.
- <code>"!roll <num_dice>d<num_sides>"</code> is shown when an inappropriate number of arguments
is given to roll.

###Helper Functions
Note that runCmd returns a <code>Net ()</code>. In order to not expose the underlying
implementation of <code>Net</code>, three functions have been provided in 
the <core>Core</core> module:
```haskell
write   :: Text -> Text -> Net ()
privmsg :: Text -> Maybe Text -> Text -> Net ()
action  :: Text -> Net ()
```
The functions do the following:
- <code>write s t</code> writes to the server with the format <code>"s t \r\n"</code>. This
function is very general, so usage is not recommended. However, this function is useful for writing
messages that are not PRIVMSG (e.g. PART, QUIT).
- <code>privmsg nick chan text</code> writes a private message to the server. If <code>chan</code> is not specified
(i.e. <code>Nothing</code>), the message is sent as a private message to the user with the nickname 
<code>nick</code>. Otherwise the message is written to the channel specified by <code>chan</code>.
- <code>action text</code> will make the bot perform an action on the channel specified by the initial
configuration.

##Plugin Processes
Sometimes it is desirable for a plugin to have persistent state that joebot2 can access.
Going in to [Core.Types](src/Joebot/Core/Types.hs)
and changing <code>Net ()</code> is not recommended as it breaks the plugin
abstraction. So how do we add new state to joebot2?

The solution is Plugin Processes. Simply put, a *Plugin Process* is an IO thread that reads the command
argument data from a channel (or channels) a la the
[Actor Model](http://en.wikipedia.org/wiki/Actor_model).
Due to the multithreaded nature of plugin processes, please note that
*you are responsible for your own race conditions*.

###Plugins.Utils Types
[Plugins/Utils.hs](src/Joebot/Plugins/Utils.hs)
provides some helper functions and types for writing your own plugin process.

The two types exported are:
```haskell
type Hook = Text -> Text -> Net ()
type Proc = Chan Msg -> IO ()
```
A <code>Hook</code> is a command that is run when a user joins, parts, or quits a channel. Currently
there are two types of hooks in Config - <code>jhooks</code> and <code>phooks</code>. The former hook is
run when a user joins and the latter when a user parts/quits.

A <code>Proc</code> is the type of a plugin process. Note that this is only a suggestion. Using
<code>Proc</code> allows you to interface with the functions exported by PluginUtils.

###Plugins.Utils Functions
The two functions exported are:
```haskell
spawnProc    :: Config 
             -> Proc 
             -> [Chan Msg -> Command] 
             -> [Chan Msg -> Hook] 
             -> [Chan Msg -> Hook] 
             -> Bool
             -> IO Config

updateConfig :: Config 
             -> Chan Msg
             -> [Chan Msg -> Command] 
             -> [Chan Msg -> Hook] 
             -> [Chan Msg -> Hook] 
             -> Bool
             -> Config
```
Both functions take in lists of commands, jhooks, and phooks.
- <code>spawnProc</code> takes a <code>Proc</code>, creates a channel, and then runs the
process. It then takes the newly created channel, applies all the commands and hooks 
with it and updates the configuration. The final boolean argument is to tell joe_bot whether
or not to send a <code>Quit</code> message to the process upon shutdown.
- <code>updateConfig</code> takes a channel and applies all the commands and hooks with it
and then updates the configuration.

#Examples
There are few example plugins packaged in with joebot2:
- [Dice](src/Joebot/Plugins/Dice/Roll.hs) is a custom command that allows joebot to roll
any number of any kind of dice. The [annotated source](http://joeschmo.github.io/joebot2/Plugins/Roll.html)
gives a detailed example of how this plugin was written and may be helpful to those
who want to write plugins of their own.
- [Mail](src/Joebot/Plugins/Mail) is a mail server that allows people to send messages to
others even when they're offline. This shows an example of how a 
[plugin process](#plugin-processes) works, along with an example of a join hook.
