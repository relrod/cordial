{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module : Cordial.Core.Types
-- Copyright : (C) 2018 Rick Elrod
-- License : BSD3 (see LICENSE file)
-- Maintainer : Rick Elrod <rick@elrod.me>
-- Stability : experimental
-- Portability : ghc
--
-- Exports core types for everything from end-user bot config to those
-- needed to write custom modules.
----------------------------------------------------------------------------
module Cordial.Core.Types where

import qualified Data.Text as T
import Discord

-- | Basic, primitive config for the bot.
--
-- Always use @defConfig@ and alter it as necessary, so that as fields get
-- added here, your bot continues to work without changes.
data CordialConfig =
  CordialConfig { botComchar :: T.Text
                , botToken :: T.Text
                , botModules :: [Module]
                }

-- | The default config for a Cordial bot.
--
-- At a minimum, you must override @botToken@.
defConfig :: CordialConfig
defConfig =
  CordialConfig { botComchar = "`"
                , botToken = ""
                , botModules = []
                }

-- | The type of a callback that can be registered by modules.
--
-- Callbacks are sent every event that the bot becomes aware of, and it is
-- up to the modules to implement the cases they care about in their callbacks
-- *and remember to gracefully disregard the rest*.
--
-- Callbacks get fired *before* command handling is executed.
data Callback =
  Callback
  Bool -- ^ Should we send everything including our own messages?
  (Dis -> Event -> IO ()) -- ^ The callback to fire

-- | Basic information for a module.
--
-- Modules extend the functionality of Cordial and allow it to react to any
-- type of event that is necessary. They get registered in the bot\'s
-- 'CordialConfig'.
data Module =
  Module { name :: T.Text -- ^ A short name for the module.
         , description :: T.Text -- ^ A short description of the module.
         , commands :: [ModuleCommand] -- ^ Commands the module exports.
         , callbacks :: [Callback] -- ^ Callbacks that events get fired to.
         }

-- | Mostly used internally, just to make it easier to work with the Discord
-- library. Exported for module convenience.
type Dis = (RestChan, Gateway, [ThreadIdType])

-- TODO: Don't be bound to IO here.
-- TODO: Store cordialConfig and Dis in the StateT.
-- | A command that is part of a module.
data ModuleCommand =
  ModuleCommand { command :: T.Text -- ^ The command name itself.
                , commandHelp :: Maybe T.Text -- ^ Help/usage text for it.
                , func :: CordialConfig -> Dis -> Message -> IO ()
                  -- ^ The actual implementation of the command.
                }
