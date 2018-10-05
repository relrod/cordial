{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module : Cordial.Module.Logger
-- Copyright : (C) 2018 Rick Elrod
-- License : BSD3 (see LICENSE file)
-- Maintainer : Rick Elrod <rick@elrod.me>
-- Stability : experimental
-- Portability : ghc
--
-- A module for logging a Discord guild.
----------------------------------------------------------------------------
module Cordial.Module.Logger where

import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Discord
import Cordial.Core.Types
import Cordial.Core.Utility
import System.FilePath.Posix ((</>))
import System.Directory (createDirectoryIfMissing)

loggerModule :: FilePath -> Module
loggerModule f =
  Module "logger" "logger for logging discord channels" [] [logMsg f]

-- In IO so we can get current timezone.
fmt :: Message -> IO T.Text
fmt msg = do
  tz <- getCurrentTimeZone
  let time = utcToLocalTime tz (messageTimestamp msg)
      formattedTime = formatTime defaultTimeLocale "%F %T" time
      nick = "<" <> (userName (messageAuthor msg)) <> ">"
      text = messageText msg
  return $ T.intercalate " " [T.pack formattedTime, T.pack nick, text, "\n"]

logMsg :: FilePath -> Callback
logMsg f = Callback True fn
  where
    fn dis (MessageCreate msg) = do
      -- TODO: We can do nicer filenames if we want. For now just use the
      -- channel snowflake.
      --files <- listDirectory f
      --let snowflakes = fmap (\file -> (takeWhile (/= '-'), file)) files
      --if messageChannel msg `elem` (fmap head snowflakes)
      createDirectoryIfMissing True f
      logLine <- fmt msg
      TIO.appendFile (f </> show (messageChannel msg)) logLine
    fn _ _ = return ()
