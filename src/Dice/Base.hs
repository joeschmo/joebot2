{-# LANGUAGE OverloadedStrings #-}
module Dice.Base (roll) where

import System.Random
import Data.Char
import Core

import qualified Data.Text as T
import Control.Lens
import Data.Text.Lens
import Data.Monoid
import Control.Monad.Trans

rollDieN :: Int -> IO Int
rollDieN n = getStdRandom $ randomR (1,n)

roll = Command "!roll" 1 rollDie "!roll <number>"

rollDie n chn args 
    | T.all isDigit (head args) = do
        i <- liftIO $ rollDieN $ read $ T.unpack $ head args
        privmsg n chn $ (show i)^.packed
    | otherwise = privmsg n chn $ n<>": invalid input"
