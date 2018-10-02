{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import System.Environment (getEnv)

import Cordial.Core
import Cordial.Core.Types
import Cordial.Core.Utility

import Cordial.Module.Demo
import Cordial.Module.Help
import Cordial.Module.Gatekeeper

preds :: [GatekeeperPredicate]
preds =
  [ MustEndIn ["@student.ysu.edu", "@ysu.edu", "@my.ysu.edu"]
  ]

cordialConfig :: String -> CordialConfig
cordialConfig tkn = defConfig { botToken = T.pack tkn
                              , botModules = [demoModule, helpModule, gatekeeperModule preds]
                              }

main :: IO ()
main = do
  tkn <- getEnv "YPH_CORDIAL"
  runCordial (cordialConfig tkn)
