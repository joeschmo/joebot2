{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Joebot.Core.Tests where

import Control.Lens

import qualified Data.Text as T

import Joebot.Core.Types
import Joebot.Core.Parse
import Data.Monoid
import Data.Char
import Control.Applicative
import Test.QuickCheck

prop_joinparse stat n ch =
  (T.length n > 0 && T.length ch > 0) ==>
  let
    to_parse = ":"<>n<>"!"<>stat<>" JOIN #"<>ch
  in
    toResponse to_parse == Join n ("#"<>ch)

prop_partparse stat n ch =
  (T.length n > 0 && T.length ch > 0) ==>
  let
    to_parse = ":"<>n<>"!"<>stat<>" PART #"<>ch
  in
    toResponse to_parse == Part n ("#"<>ch)

prop_privmsgparse stat n ch cn toks =
  (T.length n > 0 && T.length ch > 0 && T.length cn > 0
  && all (\t -> T.length t > 0) toks) ==>
  let
    to_parse = ":"<>n<>"!"<>stat<>" PRIVMSG #"<>ch<>" :"<>cn<>" "<>(T.intercalate " " toks)
  in
    toResponse to_parse == Req (Request n (Just $ "#"<>ch) cn toks) 

instance Arbitrary T.Text where
  arbitrary = 
    pure T.pack <*>
    (listOf $ suchThat arbitrary 
        (\c -> isAscii c 
            && (not $ isSpace c)
            && (c /= '!')
            && (c /= '#')))

