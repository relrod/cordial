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
import Cordial.Module.Logger
import Cordial.Module.Web

preds :: [GatekeeperPredicate]
preds =
  [ MustEndIn ["@student.ysu.edu", "@ysu.edu", "@my.ysu.edu"]
  ]

cordialConfig :: String -> CordialConfig
cordialConfig tkn = defConfig { botToken = T.pack tkn
                              , botModules = [ demoModule
                                             , helpModule
                                             --, gatekeeperModule preds
                                             , loggerModule "/home/ricky/dev/haskell/cordial/yphlogs"
                                             , webModule
                                             ]
                              }

main :: IO ()
main = do
  tkn <- getEnv "YPH_CORDIAL"
  runCordial (cordialConfig tkn)
