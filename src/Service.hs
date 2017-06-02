{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings
             , TypeOperators,
             TypeSynonymInstances #-}


module Service where

import Airport
import Groups
import Parser

-- base
import GHC.Generics (Generic)

-- aeson
import Data.Aeson (FromJSON, ToJSON)


-- servant
import Servant.API ((:>), (:<|>)(..), Capture, Get, JSON, Post, QueryParam,
                    ReqBody)
       
-- servant-server
import Servant.Server (Handler, Server, serve, errBody, err400)


-- text
import Data.Text (Text, pack)

--------------------------------------------------------------------------------
-- rest api specified as a type using Servant

--instance Generic (Stmt Airport)
--instance ToJSON (Stmt Airport)
--instance ToJSON (Program Airport)

type AirportGroupsAPI =
  "airport-groups" :> ReqBody '[String] String :> Post '[Text] [Text]
  -- "airport-groups" :> ReqBody '[JSON] (Program Airport) :> Post '[JSON] ()

--------------------------------------------------------------------------------
-- handlers

compileRun :: AirportMaps -> String -> Handler [Text]
compileRun aps progStr = do
  let out = compileAndRunProgram "" progStr aps
  return out

--------------------------------------------------------------------------------
-- server (built with servant + wai + warp)

airportGroupsServer :: AirportMaps -> Server AirportGroupsAPI
airportGroupsServer aps = compileRun aps
