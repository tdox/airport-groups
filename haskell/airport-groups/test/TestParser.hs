{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module TestParser where

import Parser

-- base
import Control.Monad (forM_)
-- import Debug.Trace

-- containers
import qualified Data.IntMap as IM
import qualified Data.Map as M

-- parsec
import Text.Parsec (ParseError, parseTest, runParser)

-- text
import Data.Text (Text, pack, unpack)

-- airport
import Airport (Airport(Airport), AirportCodes(AirportCodes), AirportIdMap
               , AirportMaps(apFaaMap), CountryAirportCode(CAC), FAA(FAA)
               , ID(ID)
               , LatLonDeg(LatLonDeg)
               , iD, apId, mkAirportMaps)
       
import Groups (Program, execProgram)


test1 :: IO ()
test1 = do
  parseTest faaIdentifier "FAA:SFO"
  parseTest icaoIdentifier "ICAO:KSFO"
  parseTest airportIdentifier "FAA:SFO"
  parseTest airportIdentifier "ICAO:KSFO"
  parseTest airportIdentifier "IATA:YSFO"
  parseTest airportIdentifiers {- $ stripSpaces -} "ICAO:KSFO, FAA:LAX"
  parseTest airportIdentifierList {- $ stripSpaces -} "{ICAO:KSFO, FAA:LAX}"
  parseTest setVar "s1_"
  parseTest setExprCode "s1_"
  parseTest setExprCode {- $ stripSpaces -} "{ICAO:KSFO}"
  parseTest setExprCode {- $ stripSpaces -} "{ICAO:KSFO, FAA:LAX}"
  --parseTest setAssignStmt $ stripSpaces "s1 = {ICAO:KSFO, FAA:LAX}"
  --parseTest prog $ stripSpaces "s1 = {ICAO:KSFO, FAA:LAX} ; s2 = {IATA:XYZ}"

testLoadAirports :: IO ()
testLoadAirports = do
  let fp = "../misc/airports_dev.txt"
  maps <- loadAirports fp
  let faas = apFaaMap maps
  forM_ (take 10 (M.toList faas)) print
  

mkTestAirportIdMap :: AirportIdMap
mkTestAirportIdMap = foldr (\ap -> IM.insert (iD (apId ap)) ap) IM.empty as
  where
    a1 = Airport (ID 1)
             (AirportCodes (Just (FAA "SFO")) Nothing Nothing Nothing)
             "USA"
             (Just ('C', 'A'))
             (LatLonDeg 33.7749 (-122.4194))
  
    a2 = Airport (ID 2)
             (AirportCodes (Just (FAA "LAX"))
                           Nothing Nothing  (Just (CAC "USA" "LAX")))
             "USA"
             (Just ('C', 'A'))
             (LatLonDeg 33.9416 (-118.4085))

    as = [a1, a2]
             


mkTestAirportMaps :: AirportMaps
mkTestAirportMaps = mkAirportMaps mkTestAirportIdMap





test2 :: IO ()
test2 = do
  let
    pStr1 = {- stripSpaces -} "s1 = {ICAO:KSFO}; print(s1)" --  ; s2 = {ICAO:KMDW}"
    fp = "./misc/airports_stg.txt"

  putStr "loading airports ..."
  aps <- loadAirports fp
  putStrLn " loaded"

  let
    ep1 :: Either ParseError (Program Airport)
    ep1 = runParser prog aps "dummySrc" pStr1

  -- print ep1

  let
    output :: [Text]
    output = case ep1 of
      Left err -> [pack $ show err]
      Right p1 -> case execProgram ([], M.empty) p1 of
        Left err -> [err]
        Right (out, _) -> out

  putStrLn $ "output:"

  forM_ output (putStrLn . unpack)
   
  putStrLn "done"


--------------------------------------------------------------------------------

