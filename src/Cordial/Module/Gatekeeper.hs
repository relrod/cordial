{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module : Cordial.Module.Gatekeeper
-- Copyright : (C) 2018 Rick Elrod
-- License : BSD3 (see LICENSE file)
-- Maintainer : Rick Elrod <rick@elrod.me>
-- Stability : experimental
-- Portability : ghc
--
-- A module implementing basic authorization. Do not rely on it for now.
--
-- Also, it is useless for everyone except YPH/YSU right now.
----------------------------------------------------------------------------
module Cordial.Module.Gatekeeper where

import Data.Monoid ((<>))
import qualified Data.Text as T
import Discord
import Cordial.Core.Types
import Cordial.Core.Utility

import Database.SQLite.Simple
import Network.Mail.SMTP
import System.Random

-------------------------------
-------- NOTE ------
-- This module is very specific to YPH/YSU right now.
-------- NOTE ------
-------------------------------

-- | Someone who has asked for verification but has yet to verify.
data PendingConfirmation =
  PendingConfirmation { pcUserid :: Snowflake
                      , pcKey :: T.Text
                      , pcValue :: T.Text
                      } deriving (Eq, Ord, Show)

-- | A predicate that the argument to the initial verification command must
-- match.
data GatekeeperPredicate =
  MustEndIn [T.Text]
  deriving Show

-- | Process a given predicate over a given 'T.Text'.
processPredicate :: T.Text -> GatekeeperPredicate -> Bool
processPredicate txt (MustEndIn suffixes) = or (fmap (\suf -> suf `T.isSuffixOf` txt) suffixes)

-- | This module. Hello!
gatekeeperModule :: [GatekeeperPredicate] -> Module
gatekeeperModule predicates =
  Module "gatekeeper" "provides basic authorization for Discord guilds" [confirmid predicates, gkPredicates predicates] [greetUser]

-- | A command that shows the predicates that the argument must match.
gkPredicates :: [GatekeeperPredicate] -> ModuleCommand
gkPredicates predicates = ModuleCommand "gkPredicates" help $ \_ dis msg -> do
  reply dis msg (T.pack $ show predicates)
  where help = Just "Displays current gatekeeper predicates"

-- | The command to start verification.
confirmid :: [GatekeeperPredicate] -> ModuleCommand
confirmid predicates = ModuleCommand "confirmid" help $ \cfg dis msg -> do
  let msgArgs = args msg
  if length msgArgs == 0
    then reply dis msg "Usage: `confirmid [valid email address]"
    else do
      g <- newStdGen
      let code = take 20 $ randomRs ('A','Z') g
          proposedId = head msgArgs
          matchesAny = any (processPredicate proposedId) predicates
          mail = simpleMail "rbelrod@student.ysu.edu" ["rick@elrod.me"] [] [] "test email" [plainTextPart "Hello!"]
      if not matchesAny
        then reply dis msg "Your proposed identity does not match any of the required predicates."
        else do
          let authorSnowflake = userId (messageAuthor msg)
              pc = PendingConfirmation authorSnowflake proposedId (T.pack code)
          renderSendMail mail
          reply dis msg ("dispatch complete (DEBUG: " <> T.pack (show pc) <> ")")
  where help = Just "Confirm your identity."

-- | A callback that sends a message to a user when they join.
greetUser :: Callback
greetUser = Callback fn
  where
    fn dis (GuildMemberAdd snowflake member) = do
      chan <- createDM dis (memberUser member)
      case chan of
        Left msg -> print msg
        Right chan' ->
          sendMsg dis chan' "Welcome to the YPH Discord! This Discord guild is \
                            \intended for students, staff, and affiliates of \
                            \Youngstown State University. To confirm your \
                            \affiliation with the University, please verify your \
                            \account to retain access to this Discord guild. To \
                            \verify your account, type \\`confirmid YOUR_YSU_EMAIL \
                            \in this message. I will send you an email with a \
                            \unique verification code. Once you receive the email, \
                            \please send me another message, \\`verify YOUR_CODE. \
                            \Once you do that, you'll be good to go!"
    fn _ _ = return ()
