{-# LANGUAGE OverloadedStrings #-}


-- base
import Control.Monad (foldM, forM_, void)


-- containers
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)
import qualified Data.Map as M

-- parsec
import Text.Parsec (Parsec, ParseError, (<|>), (<?>), char, getState, letter, many1, parse, parseTest, runParser, skipMany, space, string, try, unexpected)

-- text
import Data.Text (Text, pack, unpack)

import AirportGroups (Airport)
import Groups (Program, execProgram)
import Parser (loadAirports, prog)


main :: IO ()
main = do
  test1


test1 :: IO ()
test1 = do
  let p1 = "prog1.txt"
  putStrLn p1
  readAndExec p1

readAndExec :: String -> IO ()
readAndExec progName = do
  let
    airportsFp = "../misc/airports_stg.txt"

  --putStr "loading airports ..."
  aps <- loadAirports airportsFp
  --putStrLn " loaded"

  let programFp = "programs/" ++ progName

  --putStr "loading program ..."
  progStr <- readFile programFp
  --putStrLn " loaded"

  let
    ep1 :: Either ParseError (Program Airport)
    ep1 = runParser prog aps programFp progStr

  -- print ep1

  let
    output :: [Text]
    output = case ep1 of
      Left err -> [pack $ show err]
      Right p1 -> case execProgram ([], M.empty) p1 of
        Left err -> [err]
        Right (out, _) -> out

  --putStrLn $ "output:"

  forM_ output (putStrLn . unpack)
   
  putStrLn "done"

  

{-
testParser1 :: IO ()
testParser1 = do
  let
    aps = mkTestAirportMaps

  putStrLn "done"

             


mkTestAirportMaps :: AirportMaps
mkTestAirportMaps = mkAirportMaps mkTestAirportIdMap

-}
