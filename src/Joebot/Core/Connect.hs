{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Joebot.Core.Connect where

import Network
import System.IO
import Control.Exception
import Control.Monad.Reader
import Control.Lens
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
    notify a = bracket_
      (printf "Connecting to %s ... " (c^.server) >> hFlush stdout)
      (putStrLn "done.")
      a

-- | Continuously read responses from the socket
-- | and evaluate them.
listen :: Handle -> Net ()
listen h = forever $ 
    (liftIO $ BS.hGetLine h) >>= 
    (return . T.init . E.decodeUtf8) >>= (\s ->
    (liftIO $ T.putStrLn s) >>
    (eval . toResponse) s)

