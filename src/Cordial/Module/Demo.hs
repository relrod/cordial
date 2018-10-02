{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module : Cordial.Module.Demo
-- Copyright : (C) 2018 Rick Elrod
-- License : BSD3 (see LICENSE file)
-- Maintainer : Rick Elrod <rick@elrod.me>
-- Stability : experimental
-- Portability : ghc
--
-- A demo module showing that Cordial can actually do something.
----------------------------------------------------------------------------
module Cordial.Module.Demo where

import qualified Data.Text as T
import Discord
import Cordial.Core.Types
import Cordial.Core.Utility

demoModule :: Module
demoModule = Module "demo" "demo module for testing command stuff"
  [ping, echo, reverseCmd, reverseWords] []

ping :: ModuleCommand
ping = ModuleCommand "ping" help $ \cfg dis msg -> do
  reply dis msg "pong"
  where help = Just "Sends a message to the channel to confirm connectivity."

echo :: ModuleCommand
echo = ModuleCommand "echo" help $ \cfg dis msg -> do
  print "echo!"
  reply dis msg (T.unwords . drop 1 . T.words . messageText $ msg)
  where help = Just "Echoes back your message to you, for maximum lols."

reverseCmd :: ModuleCommand
reverseCmd = ModuleCommand "reverse" help $ \cfg dis msg -> do
  reply dis msg (T.reverse . T.unwords . drop 1 . T.words . messageText $ msg)
  where help = Just "Echoes back your message to you, reversed."

reverseWords :: ModuleCommand
reverseWords = ModuleCommand "reversewords" help $ \cfg dis msg -> do
  reply dis msg (T.unwords . reverse . drop 1 . T.words . messageText $ msg)
  where help = Just "Echoes back your message to you, with word order reversed."
