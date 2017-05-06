{-# LANGUAGE OverloadedStrings #-}



-- containers

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)
import qualified Data.Map as M


import AirportGroups

{-
type AirportIdMap = IntMap Airport
type AirportFaaMap = Map FAA Airport
type AirportIcaoMap = Map ICAO Airport

data AirportMaps = AirportMaps { apIdMap   :: AirportIdMap
                               , apFaaMap  :: AirportFaaMap
                               , apIcaoMap :: AirportIcaoMap
                               }


mkFaaMap :: AirportIdMap -> AirportFaaMap
mkFaaMap idMap = foldr (\(_, ap) -> case acFAA (apCodes ap) of
                               Nothing -> id
                               Just faa -> M.insert faa ap)
                        M.empty
                        (IM.toList idMap)

mkIcaoMap :: AirportIdMap -> AirportIcaoMap
mkIcaoMap idMap = foldr (\(_, ap) -> case acICAO (apCodes ap) of
                               Nothing -> id
                               Just icao -> M.insert icao ap)
                        M.empty
                        (IM.toList idMap)

mkAirportMaps :: AirportIdMap -> AirportMaps
mkAirportMaps idMap = AirportMaps idMap (mkFaaMap idMap) (mkIcaoMap idMap)
-}

main :: IO ()
main = putStrLn "Test suite not yet implemented"




testParser1 :: IO ()
testParser1 = do

  let
    aps = mkTestAirportMaps


  putStrLn "done"

mkTestAirportIdMap :: AirportIdMap
mkTestAirportIdMap = foldr (\ap -> IM.insert (iD (apId ap)) ap) IM.empty as
  where
    a1 = Airport (ID 1)
             (AirportCodes (Just (FAA "SFO")) Nothing Nothing Nothing)
             "USA"
             (Just ('C', 'A'))
             (33.7749, (-122.4194))
  
    a2 = Airport (ID 2)
             (AirportCodes (Just (FAA "LAX"))
                           Nothing Nothing  (Just (CAC "USA" "LAX")))
             "USA"
             (Just ('C', 'A'))
             (33.9416, (-118.4085))

    as = [a1, a2]
             


mkTestAirportMaps :: AirportMaps
mkTestAirportMaps = mkAirportMaps mkTestAirportIdMap
