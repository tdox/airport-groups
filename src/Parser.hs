{-# LANGUAGE OverloadedStrings #-}

module Parser where

import AirportGroups

-- parsec
import Text.Parsec (Parsec, (<|>), letter, many1, parseTest, string)

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
airportCodeIdentifier = undefined -- faaIdentifier <|> icaoIdentifier <|> iataIdentifier


test1 :: IO ()
test1 = do
  parseTest faaIdentifier "FAA:SFO"
  parseTest icaoIdentifier "ICAO:KSFO"

