-----------------------------------------------------------------------------
-- |
-- Module : Cordial.Core
-- Copyright : (C) 2018 Rick Elrod
-- License : BSD3 (see LICENSE file)
-- Maintainer : Rick Elrod <rick@elrod.me>
-- Stability : experimental
-- Portability : ghc
--
-- Provides core functions for a Cordial bot to... function.
----------------------------------------------------------------------------
module Cordial.Core where

import Control.Exception (finally)
import Control.Monad (forever, when)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Discord

import Cordial.Core.Types
import Cordial.Core.Utility

-- This would be way nicer with lenses.
pushEventToModules :: Dis -> Event -> [Module] -> IO ()
pushEventToModules dis evt mods = do
  print $ "Firing " <> show evt <> " to modules"
  mapM_ (f . callbacks) mods
  where
    f = mapM_ (\(Callback cb) -> cb dis evt)

mainLoop :: CordialConfig -> Dis -> IO ()
mainLoop cordialConfig dis = do
  let modules = botModules cordialConfig
  forever $ do
    e <- nextEvent dis
    pushEventToModules dis e modules
    case e of
      MessageCreate m -> handleMessage cordialConfig dis m
      _ -> return ()

runCordial :: CordialConfig -> IO ()
runCordial cordialConfig = do
  let tok = botToken cordialConfig
  dis <- loginRestGateway (Auth tok)
  finally (mainLoop cordialConfig dis) (stopDiscord dis)

handleMessage :: CordialConfig -> Dis -> Message -> IO ()
handleMessage cordialConfig dis m = do
  -- Call relvant commands from all modules
  let w = T.words . messageText $ m
      modules = botModules cordialConfig
  when (length w > 0) $ do
    let allCommands = modules >>= commands
        userCmd = head w
        comChar = botComchar cordialConfig
        matchingCommands = filter (\c -> comChar <> command c == userCmd) allCommands
    mapM_ (\c -> (func c) cordialConfig dis m) matchingCommands

