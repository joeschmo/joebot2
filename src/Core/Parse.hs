{-# LANGUAGE OverloadedStrings #-}
module Core.Parse (toResponse) where

import Control.Lens

import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Combinator as AC
import qualified Data.Text as T
import Debug.Trace

import Data.Monoid
import Control.Applicative

import Core.Types

-- |Takes a 'Text' and parses it into a 'Response'
toResponse s = 
    case A.parseOnly parseResponse s of
         Left err  -> Txt s
         Right res -> res

parseResponse =
            (A.try parsePing) <|>
            (A.try parseJoin) <|> 
            (A.try parsePart) <|>
            (A.try parseQuit) <|>
            parseReq

parseNick toRes = toRes <$>
    (A.char ':' *>
     A.takeWhile (/= '!'))

parseStat stat =
    (A.skipWhile (/= ' ') *>
     A.space *>
     A.string stat)

parseChan chanf =
    (A.skipWhile (/= '#') *>
     A.char '#' *>
     pure chanf <*> (pure (T.cons '#')
                <*> A.takeWhile (/= ' ')))

parsePing = Ping <$>
    (A.string "PING" *>
     A.space *>
     A.char ':' *>
     A.takeText)

parseStatus toRes stat = 
    parseNick toRes <*>
    (parseStat stat *>
     parseChan id)

parseJoin = parseStatus Join "JOIN"

parsePart = parseStatus Part "PART"

parseQuit = parseStatus Part "QUIT"

parseReq = Req <$>  
    (parseNick Request <*>
    (parseStat "PRIVMSG" *>
     ((A.try $ parseChan Just) 
     <|>
     (pure Nothing))) <*>
    (A.skipWhile (/=':') *>
     A.char ':' *>
     A.takeWhile (/= ' ')) <*>
    ((A.try (A.space *>
      pure T.words <*> A.takeText)) <|>
      pure []))
