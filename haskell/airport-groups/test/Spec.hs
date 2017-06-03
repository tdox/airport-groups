{-# LANGUAGE OverloadedStrings #-}

-- base
import Control.Monad (foldM, forM_, void)

-- containers
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

-- parsec
import Text.Parsec (Parsec, ParseError, (<|>), (<?>), char, getState
                   , letter, many1, parse, parseTest, runParser, skipMany
                   , space, string, try, unexpected)

-- text
import Data.Text (Text, pack, unpack)

import Airport (Airport, AirportMaps, AirportCode(FAAac, IATAac), FAA(FAA)
               , IATA(IATA), apLatLon, distanceBetweenAirports, distanceMiles)
       
import qualified Airport as AP
       
import Groups (Program, Val(SetVal), Store, execProgram)
import Parser (loadAirports, compileAndRunProgram, mkInitialAirportStore, prog)



main :: IO ()
main = do
  test1


test1 :: IO ()
test1 = do
  let
    airportsFp = "./misc/airports_dev.txt"
    -- airportsFp = "./misc/airports_stg.txt"
    
  aps <- loadAirports airportsFp

  -- testDistance aps
  
  let p1 = "prog10.txt"
  --putStrLn p1
  readAndExec aps p1


readAndExec :: AirportMaps -> String -> IO ()
readAndExec aps progName = do
  let programFp = "./test/programs/" ++ progName
  progStr <- readFile programFp
  let output = compileAndRunProgram programFp progStr aps
  forM_ output (putStrLn . unpack)
   
--  putStrLn "done"


getAirport :: AirportMaps -> Text -> Airport
getAirport aps faa = ap
  where
    Just ap = AP.lookup (FAAac (FAA faa)) aps


testDistance :: AirportMaps -> IO () 
testDistance aps = do
  let
    [sfo, teb, sjc] = map (getAirport aps) ["SFO", "TEB", "SJC"]

    Just nrt = AP.lookup (IATAac (IATA "NRT")) aps

    d1 = distanceBetweenAirports sfo teb
    d2 = distanceBetweenAirports sfo sjc
    d3 = distanceBetweenAirports sfo nrt

  putStrLn $ "sfo - teb: " ++ show d1
  putStrLn $ "sfo - sjc: " ++ show d2
  putStrLn $ "sfo - nrt: " ++ show d3

