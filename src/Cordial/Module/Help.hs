{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module : Cordial.Module.Help
-- Copyright : (C) 2018 Rick Elrod
-- License : BSD3 (see LICENSE file)
-- Maintainer : Rick Elrod <rick@elrod.me>
-- Stability : experimental
-- Portability : ghc
--
-- A module that provides help for all other loaded modules.
----------------------------------------------------------------------------
module Cordial.Module.Help where

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Discord
import Cordial.Core.Types
import Cordial.Core.Utility

helpModule :: Module
helpModule =
  Module "help" "provides help and information for modules' commands" [modulesCmd, helpCmd, commandsCmd] []

modulesCmd :: ModuleCommand
modulesCmd = ModuleCommand "modules" help $ \cfg dis msg -> do
  let mods = fmap (\m -> "  • " <> name m <> " (" <> description m <> ")") (botModules cfg)
      text = "Enabled modules:\n" <> T.unlines mods
  reply dis msg text
  where help = Just "Lists enabled modules."

helpCmd :: ModuleCommand
helpCmd = ModuleCommand "help" help $ \cfg dis msg -> do
  let msgArgs = args msg
  if length msgArgs > 0
    then
      let query = head msgArgs
          modules = botModules cfg
          allCommands = modules >>= commands
          matchingCommands = filter (\c -> command c == query) allCommands
        in if length matchingCommands /= 0
           then mapM_ (\c -> reply dis msg (fromMaybe "Command does not provide help information" (commandHelp c))) matchingCommands
           else reply dis msg ("No such command: " <> query)
    else reply dis msg "You didn't tell me what to look up!"
  where help = Just "Looks up a command's help text."

commandsCmd :: ModuleCommand
commandsCmd = ModuleCommand "commands" help $ \cfg dis msg -> do
  let mods = filter (\m -> not . null  $ commands m) (botModules cfg)
  reply dis msg (T.intercalate ", " $
                  fmap (\m -> name m <> " (" <> T.intercalate ", " (fmap command (commands m)) <> ")") mods)
  where help = Just "Lists commands in each enabled module."
