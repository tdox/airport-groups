{-# LANGUAGE OverloadedStrings #-}

-- base
import Control.Monad (foldM, forM_, void)

-- containers
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)
import qualified Data.Map as M

-- parsec
import Text.Parsec (Parsec, ParseError, (<|>), (<?>), char, getState
                   , letter, many1, parse, parseTest, runParser, skipMany
                   , space, string, try, unexpected)

-- text
import Data.Text (Text, pack, unpack)

import Airport (Airport, AirportMaps, AirportCode(FAAac, IATAac), FAA(FAA)
               , IATA(IATA), apLatLon, distanceBetweenAirports, distanceMiles)
       
import qualified Airport as AP
       
import Groups (Program, execProgram)
import Parser (loadAirports, prog)



main :: IO ()
main = do
  test1


test1 :: IO ()
test1 = do
  let
    airportsFp = "./misc/airports_dev.txt"
    
  aps <- loadAirports airportsFp

  testDistance aps

  {-
  let p1 = "prog6.txt"
  putStrLn p1
  readAndExec aps p1
-}

readAndExec :: AirportMaps -> String -> IO ()
readAndExec aps progName = do

  let programFp = "./test/programs/" ++ progName

  progStr <- readFile programFp
  putStrLn $ "progamFp:"
  putStrLn progStr

  let
    ep1 :: Either ParseError (Program Airport)
    ep1 = runParser prog aps programFp progStr

  putStrLn "ep1:"
  print ep1

  let
    output :: [Text]
    output = case ep1 of
      Left err -> [pack $ show err]
      Right p1 -> case execProgram ([], M.empty) p1 of
        Left err -> [err]
        Right (out, _) -> out

  putStrLn $ "nLines: " ++ show (length output)
  forM_ output (putStrLn . unpack)
   
  putStrLn "done"


testDistance :: AirportMaps -> IO () 
testDistance aps = do
  let
    {-
    Just sfo = AP.lookup (FAAac (FAA "SFO")) aps
    Just teb = AP.lookup (FAAac (FAA "TEB")) aps
    Just sjo = AP.lookup (FAAac (FAA "SJO")) aps
-}
    [sfo, teb, sjc] = map (getAirport aps) ["SFO", "TEB", "SJC"]

    Just nrt = AP.lookup (IATAac (IATA "NRT")) aps

    d1 = distanceBetweenAirports sfo teb
    d2 = distanceBetweenAirports sfo sjc
    d3 = distanceBetweenAirports sfo nrt

  putStrLn $ "sfo - teb: " ++ show d1
  putStrLn $ "sfo - sjc: " ++ show d2
  putStrLn $ "sfo - nrt: " ++ show d3



getAirport :: AirportMaps -> Text -> Airport
getAirport aps faa = ap
  where
    Just ap = AP.lookup (FAAac (FAA faa)) aps
