{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings
             , TypeOperators,
             TypeSynonymInstances #-}


module Service where

import Airport
--import Groups
import Parser

-- base
-- import Data.Proxy (Proxy(Proxy))
import GHC.Generics (Generic)

-- aeson
import Data.Aeson (FromJSON, ToJSON)

import Servant

-- servant
import Servant.API ((:>), (:<|>), JSON, Post, Raw, ReqBody)
       
-- servant-server
import Servant.Server (Handler, Server, serve)


-- text
import Data.Text (Text, unpack)

-- wai
import Network.Wai (Application)

-- wai-cors
import Network.Wai.Middleware.Cors (CorsResourcePolicy(corsRequestHeaders)
                                   , cors, simpleCorsResourcePolicy)

-- warp
import Network.Wai.Handler.Warp (run)

--------------------------------------------------------------------------------
-- rest api specified as a type using Servant


data SourceCode = SourceCode {src :: Text} deriving Generic
data ProgOutput = ProgOutput {out :: [Text]} deriving Generic

instance FromJSON SourceCode
instance ToJSON ProgOutput

type AirportGroupAPI =
  "airport-group" :> ReqBody '[JSON] SourceCode :> Post '[JSON] ProgOutput
   :<|> "web-app" :> Raw

--------------------------------------------------------------------------------
-- handlers

compileRun :: AirportMaps -> SourceCode -> Handler ProgOutput
compileRun aps (SourceCode txt) = do
  let o = compileAndRunProgram "" (unpack txt) aps :: [Text]
  return $ ProgOutput o

options :: AirportMaps -> SourceCode -> Handler ()
options _ _ = return ()

--------------------------------------------------------------------------------
-- server (built with servant + wai + warp)

--airportGroupServer :: AirportMaps -> Server AirportGroupAPI
--airportGroupServer aps = undefined -- compileRun aps

server :: AirportMaps -> Server AirportGroupAPI
server aps = serveInterpreter :<|> serveHtml
  where
    serveInterpreter = compileRun aps -- undefined :: 
    serveHtml = serveDirectory "/tmp/apg"

airportGroupAPI :: Proxy AirportGroupAPI
airportGroupAPI = Proxy

app :: AirportMaps -> Application
-- app aps = serve airportGroupAPI $ airportGroupServer aps
app aps = serve airportGroupAPI $ server aps

--------------------------------------------------------------------------------
-- server-io

service :: IO ()
service = do
  let
    port = 8080
    airportsFp = "./misc/airports_stg.txt"

  putStr "loading airports..."
  aps <- loadAirports airportsFp
  putStrLn " done"

  putStrLn $ "airport-group-service running on " ++ show port

  run port $ cors (const $ Just policy) $ app aps
    where
      policy =
        simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type"] }
-- See: https://github.com/haskell-servant/servant-swagger/issues/45
