{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Joebot.Core.Tests where

import Control.Lens

import qualified Data.Text as T

import Joebot.Core.Types
import Joebot.Core.Parse
import Data.Monoid
import Data.Char
import Test.QuickCheck

prop_joinparse stat n ch =
  (T.length n > 0 && T.length ch > 0) ==>
  let
    to_parse = ":"<>n<>"!"<>stat<>" JOIN #"<>ch
  in
    case toResponse to_parse of
      Join _ _ -> True
      _        -> False

prop_partparse stat n ch =
  (T.length n > 0 && T.length ch > 0) ==>
  let
    to_parse = ":"<>n<>"!"<>stat<>" PART #"<>ch
  in
    case toResponse to_parse of
      Part _ _ -> True
      _        -> False

prop_privmsgparse stat n ch msg =
  (T.length n > 0 && T.length ch > 0) ==>
  let
    to_parse = ":"<>n<>"!"<>stat<>" PRIVMSG #"<>ch<>" :"<>msg
  in
    case toResponse to_parse of
      Req _ -> True
      _     -> False

instance Arbitrary T.Text where
  arbitrary = 
    (listOf $ suchThat arbitrary (not . isSpace)) 
    >>= return . T.pack

