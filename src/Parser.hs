{-# LANGUAGE OverloadedStrings #-}

module Parser where


-- base
import Control.Monad (void)
import Data.Char (isSpace)

-- containers
import Data.Set (fromList)

-- parsec
import Text.Parsec (Parsec, (<|>), char, letter, many1, parseTest, string, try)
import qualified Text.Parsec.Token as P -- (commaSep1, makeTokenParser)
import Text.Parsec.Language (haskellDef)

-- text
import Data.Text (Text, pack)

import AirportGroups
import Groups

--------------------------------------------------------------------------------

lexer = P.makeTokenParser haskellDef

faaIdentifier :: Parsec String st FAA
faaIdentifier = do
  string "FAA:"
  code <- many1 letter
  return $ FAA $ pack code

icaoIdentifier :: Parsec String st ICAO
icaoIdentifier = do
  string "ICAO:"
  code <- many1 letter
  return $ ICAO $ pack code

iataIdentifier :: Parsec String st IATA
iataIdentifier = do
  string "IATA:"
  code <- many1 letter
  return $ IATA $ pack code


airportIdentifier :: Parsec String st AirportCode
airportIdentifier =
  try (do
    faa <- faaIdentifier
    return $ FAAac faa)
  <|> try (do
    icao <- icaoIdentifier
    return $ ICAOac icao)
  <|> (do
    iata <- iataIdentifier
    return $ IATAac iata)

commaSep1 = P.commaSep1 lexer
squares = P.squares lexer
identifier = P.identifier lexer

airportIdentifiers :: Parsec String st [AirportCode]
airportIdentifiers = commaSep1 airportIdentifier

airportIdentifierList :: Parsec String st [AirportCode]
airportIdentifierList = squares airportIdentifiers

stripSpaces :: String -> String
stripSpaces str = filter (not . isSpace) str

setVar :: Parsec String st Var
setVar = pack <$> identifier

setExpr :: Parsec String st (SetExpr AirportCode)
setExpr =
  try (SVar <$> setVar)
  <|> try ((Elems . fromList) <$> airportIdentifierList)


setAssignStmt :: Parsec String st (Stmt AirportCode)
setAssignStmt = do
  var <- setVar
  void $ char '='
  expr <-setExpr
  return $ AssignSet var expr
    
--setVar :: Parsec String st (AssignSetStmt AirportCode)
--setVar = undefined

  

test1 :: IO ()
test1 = do
  parseTest faaIdentifier "FAA:SFO"
  parseTest icaoIdentifier "ICAO:KSFO"
  parseTest airportIdentifier "FAA:SFO"
  parseTest airportIdentifier "ICAO:KSFO"
  parseTest airportIdentifier "IATA:YSFO"
  parseTest airportIdentifiers $ stripSpaces "ICAO:KSFO , FAA:LAX"
  parseTest airportIdentifierList $ stripSpaces "[  ICAO:KSFO , FAA:LAX  ]"
  parseTest setVar "s1_"
  parseTest setExpr "s1_"
  parseTest setExpr $ stripSpaces "[ ICAO : KSFO ]"
  parseTest setExpr $ stripSpaces "[ ICAO : KSFO, FAA:LAX ]"
  parseTest setAssignStmt $ stripSpaces "s1 = [ICAO:KSFO, FAA:LAX]"

