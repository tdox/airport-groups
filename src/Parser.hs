{-# LANGUAGE OverloadedStrings #-}

module Parser where


-- base
import Control.Monad (void)
import Data.Char (isSpace)

-- containers
import Data.Set (fromList)
import qualified Data.IntMap as IM

-- parsec
import Text.Parsec (Parsec, ParseError, (<|>), char, letter, many1, parse, parseTest, string, try)
import qualified Text.Parsec.Token as P -- (commaSep1, makeTokenParser)
import Text.Parsec.Language (haskellDef)

-- text
import Data.Text (Text, pack)

import AirportGroups
import Groups

--------------------------------------------------------------------------------
note: change AirportCode to Airport

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
semiSep1 = P.semiSep1 lexer

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
  expr <- setExpr
  return $ AssignSet var expr

-- predAssignStmt ::

printStmt :: Parsec String st (Stmt AirportCode)
printStmt = do
  void $ string "print("
  --api <- airportIdentifier
  --void $ char ','
  var <- setVar
  void $ char ')'
  return $ Print var
  

stmt :: Parsec String st (Stmt AirportCode)
stmt =
  try (setAssignStmt)

  

prog :: Parsec String st (Program AirportCode)
prog = semiSep1 stmt

    
--------------------------------------------------------------------------------

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
  parseTest prog $ stripSpaces "s1 = [ICAO:KSFO, FAA:LAX] ; s2 = [IATA:XYZ]"


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


test2 :: IO ()
test2 = do
  let
    pStr1 = stripSpaces "s1 = [ICAO:KSFO, FAA:LAX] ; s2 = [IATA:XYZ]"
    aps = mkTestAirportMaps

    ep1 :: Either ParseError (Program AirportCode)
    ep1 = parse prog "dummySrc" pStr1

  case ep1 of
    Left err -> print err
    Right p1 -> execProg aps p1
        
    
    
  putStrLn "done"



convertSetExpr :: AirportMaps -> SetExpr AirportCode -> SetExpr Airport
convertSetExpr aps setExprAC = undefined

execProg :: AirportMaps -> Program AirportCode -> IO ()
execProg aps p = undefined


execSt :: AirportMaps -> (Output, Store Airport) -> Stmt AirportCode -> Either Err (Output, Store Airport)
execSt aps (out0, st0) stmt = undefined
{-
  where
    eOutStore = execStmt stmt (out0, st0)

    eos = case eOutStr of
      Left err -> Left err
      Right 
  
-}
