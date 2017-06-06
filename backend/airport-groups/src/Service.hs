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
import Data.Proxy (Proxy(Proxy))
import GHC.Generics (Generic)

-- aeson
import Data.Aeson (FromJSON, ToJSON)


-- servant
import Servant.API ((:>), (:<|>)(..), Capture, Get, JSON, PlainText, Post, QueryParam,
                    ReqBody, StdMethod(OPTIONS), Verb)
       
-- servant-server
import Servant.Server (Handler, Server, serve, errBody, err400)


-- text
import Data.Text (Text, pack, unpack)

-- transformers
import Control.Monad.IO.Class (liftIO)

-- wai
import Network.Wai (Application)

-- warp
import Network.Wai.Handler.Warp (run)

--------------------------------------------------------------------------------
-- rest api specified as a type using Servant

--instance Generic (Stmt Airport)
--instance ToJSON (Stmt Airport)
--instance ToJSON (Program Airport)

data SourceCode = SourceCode {src :: Text} deriving Generic
data ProgOutput = ProgOutput {out :: [Text]} deriving Generic

instance FromJSON SourceCode
instance ToJSON ProgOutput

type AirportGroupAPI =
  "airport-group" :> ReqBody '[JSON] SourceCode :> Post '[JSON] ProgOutput
--  :<|> "airport-group" :> ReqBody '[JSON] SourceCode :> (Verb 'OPTIONS 405) '[PlainText] ()
  -- "airport-groups" :> ReqBody '[String] String :> Post '[Text] [Text]
  -- "airport-groups" :> ReqBody '[JSON] (Program Airport) :> Post '[JSON] ()

--------------------------------------------------------------------------------
-- handlers

-- compileRun :: AirportMaps -> String -> Handler [Text]
compileRun :: AirportMaps -> SourceCode -> Handler ProgOutput
compileRun aps (SourceCode txt) = do
--  liftIO $ putStrLn "compileRun"
  let o = compileAndRunProgram "" (unpack txt) aps :: [Text]
  return $ ProgOutput o

options :: AirportMaps -> SourceCode -> Handler ()
options _ _ = return ()

--------------------------------------------------------------------------------
-- server (built with servant + wai + warp)

airportGroupServer :: AirportMaps -> Server AirportGroupAPI
airportGroupServer aps = compileRun aps -- :<|> options aps
--  where
  --  x = compileRun aps :: _

--server  :: AirportMaps -> Server API
--server aps = return $ airportGroupsServer aps

airportGroupAPI :: Proxy AirportGroupAPI
airportGroupAPI = Proxy

--api :: Proxy API
--api = Proxy

app :: AirportMaps -> Application
app aps = serve airportGroupAPI $ airportGroupServer aps

--------------------------------------------------------------------------------
-- server-io

service :: IO ()
service = do
--  writeSwaggerJSON -- should really be in a separate main
  let
    port = 8080
    airportsFp = "./misc/airports_dev.txt"

  putStr "loading airports..."
  aps <- loadAirports airportsFp
  putStrLn " done"

--  planes <- emptyTableIO
  putStrLn $ "airport-group-service running on " ++ show port
--  putStrLn $ "View the API docs with swagger-ui at http://<SERVER>:" ++ (show port) ++ "/swagger.json"
  run port $ app aps

-- TODO TD 6/6/17 add
-- run port $ cors (const $ Just policy) $ app aps
-- where policy = simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type"] }
-- See: https://github.com/haskell-servant/servant-swagger/issues/45
