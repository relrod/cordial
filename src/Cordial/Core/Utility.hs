{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module : Cordial.Core.Utility
-- Copyright : (C) 2018 Rick Elrod
-- License : BSD3 (see LICENSE file)
-- Maintainer : Rick Elrod <rick@elrod.me>
-- Stability : experimental
-- Portability : ghc
--
-- Useful utility functions that modules can make use of for doing
-- various things. Think of this as a tiny stdlib.
----------------------------------------------------------------------------
module Cordial.Core.Utility where

import Data.Monoid ((<>))
import qualified Data.Text as T
import Discord
import Cordial.Core.Types (Dis)

-- | Parse out the arguments of a message, dropping the first word
-- (usually the command name with comchar).
args :: Message -> [T.Text]
args msg = drop 1 . T.words . messageText $ msg

-- | Send a message to a particular channel.
sendMsg :: Dis -> Snowflake -> T.Text -> IO ()
sendMsg dis snowflake msg = do
  resp <- restCall dis (CreateMessage snowflake msg Nothing)
  putStrLn $ "[SENDING] " ++ show resp

-- | Reply to a message (takes a message and sends a message to the channel
-- from which it came).
reply :: Dis -> Message -> T.Text -> IO ()
reply dis msg = sendMsg dis (messageChannel msg)

-- | Create a DM with a user.
createDM :: Dis -> User -> IO (Either String Snowflake)
createDM dis user = do
  resp <- restCall dis (CreateDM (userId user))
  putStrLn $ "[DM CREATE] " ++ show resp
  return (fmap channelId resp)

-- | Truncate a 'T.Text' if it is too long.
trunc :: Int -> T.Text -> T.Text
trunc len t =
  if T.length t > len
  then T.take len t <> "..."
  else t
