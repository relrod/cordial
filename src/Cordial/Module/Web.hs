{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module : Cordial.Module.Web
-- Copyright : (C) 2018 Rick Elrod
-- License : BSD3 (see LICENSE file)
-- Maintainer : Rick Elrod <rick@elrod.me>
-- Stability : experimental
-- Portability : ghc
--
-- A module to look stuff up on the 'net.
----------------------------------------------------------------------------
module Cordial.Module.Web where

import Data.Aeson
import Data.Monoid ((<>))
import qualified Data.Text as T
import Discord
import Network.HTTP.Simple

import Cordial.Core.Types
import Cordial.Core.Utility

webModule :: Module
webModule = Module "web" "look stuff up on the web"
  [urban] []

data UrbanDictDef =
  UrbanDictDef { udDefinition :: T.Text
               , udPermalink :: T.Text
               , udThumbsUp :: Integer
               , udThumbsDown :: Integer
               , udSoundUrls :: [T.Text]
               , udAuthor :: T.Text
               , udWord :: T.Text
               , udDefId :: Integer
               , udCurrentVote :: T.Text
               , udWrittenOn :: T.Text
               , udExample :: T.Text
               } deriving (Eq, Ord, Show)

newtype UrbanDictResp = UrbanDictResp [UrbanDictDef]

instance FromJSON UrbanDictDef where
  parseJSON = withObject "UrbanDictDef" $ \v ->
    UrbanDictDef <$> v .: "definition"
                 <*> v .: "permalink"
                 <*> v .: "thumbs_up"
                 <*> v .: "thumbs_down"
                 <*> v .: "sound_urls"
                 <*> v .: "author"
                 <*> v .: "word"
                 <*> v .: "defid"
                 <*> v .: "current_vote"
                 <*> v .: "written_on"
                 <*> v .: "example"

instance FromJSON UrbanDictResp where
  parseJSON = withObject "UrbanDictResp" $ \v ->
    UrbanDictResp <$> v .: "list"

urban :: ModuleCommand
urban = ModuleCommand "urban" help $ \cfg dis msg -> do
  let msgArgs = args msg
  if length msgArgs > 0
    then do
      let query = T.unwords msgArgs
      req <- parseRequest ("https://api.urbandictionary.com/v0/define?term=" <> T.unpack query)
      resp <- httpJSON req :: IO (Response UrbanDictResp)
      let urb = getResponseBody resp
      reply dis msg (fmt urb)
    else reply dis msg "Whachu want me to look up?"
  where
    help = Just "Looks up a thing on Urban Dictionary."
    fmt (UrbanDictResp lst) =
      if null lst
      then "No matches found on Urban Dictionary."
      else
        let def = head lst
        in "**" <> udWord def <> "**\n" <> trunc 750 (udDefinition def) <>
           "\n" <> (T.pack . show . udThumbsUp $ def) <> ":thumbsup:, "<>
           (T.pack . show . udThumbsDown $ def) <> ":thumbsdown:"
