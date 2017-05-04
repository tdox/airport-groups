{-# LANGUAGE OverloadedStrings #-}

module Parser where

import AirportGroups

-- parsec
import Text.Parsec (Parsec, (<|>), letter, many1, parseTest, string, try)

-- text
import Data.Text (Text, pack)

faaIdentifier :: Parsec Text st FAA
faaIdentifier = do
  string "FAA:"
  code <- many1 letter
  return $ FAA $ pack code

icaoIdentifier :: Parsec Text st ICAO
icaoIdentifier = do
  string "ICAO:"
  code <- many1 letter
  return $ ICAO $ pack code

iataIdentifier :: Parsec Text st IATA
iataIdentifier = do
  string "IATA:"
  code <- many1 letter
  return $ IATA $ pack code


airportCodeIdentifier :: Parsec Text st AirportCode
airportCodeIdentifier =  try (do
    faa <- faaIdentifier
    return $ FAAac faa)
  <|> try (do
    icao <- icaoIdentifier
    return $ ICAOac icao)
  <|> (do
    iata <- iataIdentifier
    return $ IATAac iata)

  

test1 :: IO ()
test1 = do
  parseTest faaIdentifier "FAA:SFO"
  parseTest icaoIdentifier "ICAO:KSFO"
  parseTest airportCodeIdentifier "FAA:SFO"
  parseTest airportCodeIdentifier "ICAO:KSFO"
  parseTest airportCodeIdentifier "IATA:YSFO"

