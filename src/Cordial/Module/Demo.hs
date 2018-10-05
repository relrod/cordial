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

import Data.Monoid ((<>))
import qualified Data.Text as T
import Discord
import System.Random

import Cordial.Core.Types
import Cordial.Core.Utility

demoModule :: Module
demoModule = Module "demo" "demo module for testing command stuff"
  [ping, echo, reverseCmd, reverseWords, trout, insult] []

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

trout :: ModuleCommand
trout = ModuleCommand "trout" help $ \cfg dis msg -> do
  meCall <- restCall dis GetCurrentUser
  let msgArgs = args msg
  if length msgArgs > 0
    then do
      case meCall of
        Left _ -> reply dis msg "I broke."
        Right me ->
          if length (messageMentions msg) > 0 && (userId . head . messageMentions $ msg) == userId me
          then reply dis msg "How *dare* you?"
          else reply dis msg ("_throws a trout at " <> head msgArgs <> "._")
    else reply dis msg ("_throws a trout at " <> T.pack (userName (messageAuthor msg)) <> " for not telling me who to trout._")
  where help = Just "Throws a trout at a user."

insult :: ModuleCommand
insult = ModuleCommand "insult" help $ \cfg dis msg -> do
  let insults =
        [ "I'm not trash talking, I'm talking to trash."
        , "A million years of evolution and we get you."
        , "I would insult you but nature did a better job."
        , "I'd tell you to go outside, but you'd just ruin that for everyone else too"
        , "I'd love to see things from your perspective, but I don't think I could shove my head that far up my ass."
        , "Does your ass get jealous of all the shit that comes out of your mouth?"
        , "I'm jealous of people that don't know you."
        , "What you lack in brain cells, you make up for in chromosome count."
        , "Two wrongs don't make a right, take your parents as an example."
        , "I'm sure you exist only because the other sperm felt bad."
        , "To which foundation do I need to donate to help you?"
        , "You're the reason the gene pool needs a lifeguard."
        , "Fighting on the internet is like participating in the paralympics. Even if you win, you're still retarded."
        , "If I wanted to commit suicide, I would climb up your ego and jump down to your IQ."
        , "My mind is going. I can feel it."
        , "Wrong! You cheating scum!"
        , "And you call yourself a Rocket Scientist!"
        , "And you call yourself a Rocket Surgeon!"
        , "Are you on drugs?"
        , "Your mind just hasn't been the same since the electro-shock, has it?"
        , "I don't think I can be your friend on Facebook anymore."
        , "stty: unknown mode: doofus"
        , "Listen, broccoli brains, I don't have time to listen to this trash."
        , "Have you considered trying to match wits with a rutabaga?"
        , "You speak an infinite deal of nothing."
        , "My brain just exploded"
        ]
      msgArgs = T.unwords $ args msg
  index <- randomRIO (0, length insults - 1)
  reply dis msg (msgArgs <> " " <> (insults !! index))
  where help = Just "Throw a burn at someone. Only use this if there's a fire extinguisher nearby."
