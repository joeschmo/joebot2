{-# LANGUAGE OverloadedStrings #-}
module Joebot.Core.Connect where

import Network
import System.IO
import Control.Exception
import Control.Monad.Reader
import Control.Lens
import Control.Applicative
import Text.Printf
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as BS

import Data.Monoid

import Joebot.Core.Types
import Joebot.Core.Parse
import Joebot.Core.Eval

-- | Given a configuration, connect to a server and
-- return a 'Bot'
connect :: Config -> IO Bot
connect c = notify $ do
    h <- connectTo (c^.server) $ PortNumber $ fromIntegral (c^.port)
    hSetBuffering h NoBuffering
    return $ Bot h c
  where
    notify = bracket_
      (printf "Connecting to %s ... " (c^.server) >> hFlush stdout)
      (putStrLn "done.")

-- | Continuously read responses from the socket
-- | and evaluate them.
listen :: Handle -> Net ()
listen h = forever $ 
    pure (T.init . E.decodeUtf8) <*> liftIO (BS.hGetLine h) >>=
    (\s -> liftIO (T.putStrLn s) >>
      (eval . toResponse) s)

