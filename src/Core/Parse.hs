{-# LANGUAGE OverloadedStrings #-}
module Core.Parse where

import Control.Lens

import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Combinator as AC
import qualified Data.Text as T
import Debug.Trace

import Data.Monoid
import Control.Applicative

import Core.Types
import Core.Cmds

toResponse s = 
    case A.parseOnly parseResponse s of
         Left err  -> trace err $ Txt s
         Right res -> res

parseResponse =
            (A.try parseJoin) <|> 
            (A.try parsePing) <|>
            (A.try parsePart) <|>
            parseReq

parsePing = Ping <$>
    (A.string "PING" *>
     A.space *>
     A.char ':' *>
     A.takeText)

parseJoin = Join <$>
    (A.char ':' *>
     A.takeWhile (/= '!')) <*>
    (A.skipWhile (/= ' ') *>
     A.space *>
     A.string "JOIN" *>
     A.space *>
     A.takeWhile (/= ' '))

parsePart = Part <$>
    (A.char ':' *>
     A.takeWhile (/= '!')) <*>
    (A.skipWhile (/= ' ') *>
     A.space *>
     A.string "PART" *>
     A.space *>
     A.takeWhile (/= ' ')) 
    
parseReq = Req <$>  
    (Request <$>
    (A.char ':' *>
     A.takeWhile (/= '!')) <*>
    (A.skipWhile (/= ' ') *>
     A.space *>
     A.string "PRIVMSG" *>
     (A.try (A.skipWhile (/= '#') *>
             pure Just <*> A.takeWhile (/= ' ')))
     <|>
     (pure Nothing)) <*>
    (A.space *>
     A.char ':' *>
     A.char '!' *>
     (mappend <$> pure "!" <*> A.takeWhile (/= ' '))) <*>
    ((A.try (A.space *>
      pure T.words <*> A.takeText)) <|>
      pure []))
