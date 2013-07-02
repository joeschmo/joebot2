{-# LANGUAGE OverloadedStrings #-}
module Dice.Base (roll) where

import System.Random
import Data.Char
import Core

import qualified Data.Text as T
import qualified Data.Attoparsec.Text as A
import Data.Text.Lens
import Data.Monoid

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Arrow

data Dice = Dice Int Int

rollDieN :: Int -> IO Int
rollDieN n = getStdRandom $ randomR (1,n)

roll = Command "!roll" 1 rollDie "!roll <num_dice>d<die_type>"

rollDie n chn args = do
    msg <- liftIO $ parseDice (head args)
    privmsg n chn msg

parseDice s = 
    case A.parseOnly (A.try parseD <|> parseDs) s of
      Left err -> return "invalid input"
      Right ds -> rollDice ds

parseD  = Dice <$> (pure 1) <*> (A.char 'd' *> A.decimal)
parseDs = Dice <$> A.decimal <*> (A.char 'd' *> A.decimal)


rollDice (Dice n 0) = return "zero sided dice have left me in despair!"
rollDice (Dice n m) = do
    rolls <- replicateM n (rollDieN m)
    let res = map (T.pack . show) rolls
    let total = (T.pack . show . sum) rolls
    return $ T.unwords res <> " | sum: " <> total
    
