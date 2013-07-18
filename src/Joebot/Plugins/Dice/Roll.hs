-- This language pragma allows 
-- GHC to
-- treat string literals as Text
-- ##imports
{-# LANGUAGE OverloadedStrings #-}

module Joebot.Plugins.Dice.Roll (roll) where

-- Import the pseudorandom number generator
-- used to simulate dice rolls.
import System.Random

-- This contains 
-- [<code>Core.Types</code>](https://github.com/joeschmo/joebot2/blob/master/src/Core/Types.hs)
-- and 
-- [<code>Core.Cmds</code>](https://github.com/joeschmo/joebot2/blob/master/src/Core/Cmds.hs).
-- It is mostly used for the type <code>Command</code> 
-- along with the
-- <code>privmsg</code>, 
-- <code>action</code>, and 
-- <code>write</code> functions.
import Joebot.Core

-- [<code>Data.Text</code>](http://hackage.haskell.org/packages/archive/text/0.11.3.1/doc/html/Data-Text.html)
-- supports Unicode, 
-- so it's preferred
-- over the base <code>String</code> type. 
-- Also joebot2 uses
-- <code>Text</code> extensively.
import qualified Data.Text as T
-- This is mostly used for <code>(<>)</code>, which
-- is a generic way of doing concatenation.
-- This way the code will not be littered with
-- <code>T.append</code>s. For example,
-- <pre><code>T.append s t</code></pre>
-- becomes
-- <pre><code>s <> t</code></pre>
-- when using <code>Data.Monoid</code>.
import Data.Monoid


-- [Attoparsec](http://hackage.haskell.org/packages/archive/attoparsec/0.10.4.0/doc/html/Data-Attoparsec-Text.html)
-- is a parser combinator library. It is used here
-- to help parse arguments to the [roll command](#rollcmd), 
-- which expects a certain format.
import qualified Data.Attoparsec.Text as A
-- This is a general library that helps
-- compact Attoparsec code, and reduces the 
-- use of monads. For more information about this
-- library and <code>Data.Monoid</code>, refer to
-- Brent Yorgey's [Typeclassopedia](http://www.haskell.org/haskellwiki/Typeclassopedia).
import Control.Applicative

import Control.Monad
-- The <code>liftIO</code> function allows the running
-- of IO actions in the <code>Net</code> monad
-- ##T<a id="rollcmd"></a>he roll command
import Control.Monad.Trans (liftIO)

-- This command is invoked when somebody invokes
-- "!roll" on the channel.
-- The arguments to Command are the command name, the arity,
-- the function to run, and the help message.
roll = Command "!roll" 1 rollDie "!roll <num_dice>d<die_type>"

-- This function is invoked when the roll command is run.
rollDie n chn args = do
    -- Take in the first argument and parse it.
    -- Arity is checked before this command is executed, so
    -- <code>args</code> won't be empty.
    msg <- liftIO $ parseEvalDice (head args)
    -- Print out the result of <code>parseEvalDice</code>
    -- to the IRC server
    -- ##parser
    privmsg n chn msg

-- Take in a string and try to parse out the number
-- dice and the number of sides per die.
parseEvalDice s = 
    -- Try to parse either "dX" or "XdX"
    case A.parseOnly (A.try parseDie <|> parseDice) s of
      -- If the parse fails, return "invalid input"
      Left err -> return "invalid input"
      -- If it succeeds first check the arguments to see if they're logical.
      -- If they are, then roll the dice.
      Right (n, m) -> 
        if n < 0 || m < 1 
        then return "theoretically impossible rolls have left me in despair!"
        else rollDice n m

-- A parser for "dX" where X is a number. This results in a 1dX roll.
parseDie  = (,) <$> (pure 1) <*> (A.char 'd' *> A.decimal)
-- A parser for "XdX".
-- ##Dice Rolling
parseDice = (,) <$> A.decimal <*> (A.char 'd' *> A.decimal)

-- The code for rolling the die, the arguments are
-- number of dice and number of sides, respectively.
rollDice n m = do
    -- Do n rolls of m sides. <code>replicateM</code>
    -- performs a monadic action _n_ number of times.
    rolls <- replicateM n (rollDieN m)
    -- <code>res</code> holds all the roll results as Text.
    let res = map (T.pack . show) rolls
    -- <code>total</code> holds the total of all the rolls as Text.
    let total = (T.pack . show . sum) rolls
    -- Return both <code>res</code> and <code>total</code> with some
    -- light formatting.
    return $ T.unwords res <> " | sum: " <> total
    
-- The actual die rolling function. Gets a random <code>Int</code>
-- from 1 to n.
rollDieN :: Int -> IO Int
rollDieN n = getStdRandom $ randomR (1,n)
