{-# LANGUAGE OverloadedStrings #-}

module Parser where


-- base
import Control.Monad (foldM, forM_, void, when)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe, maybe)
import Debug.Trace

-- containers
import Data.Set (fromList)
import qualified Data.IntMap as IM
import qualified Data.Map as M

-- parsec
import Text.Parsec (Parsec, ParseError, (<|>), (<?>), char, getInput
                   , getState, letter, many, many1, parse, parseTest, runParser
                   , skipMany, space, string, try, unexpected)

import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

-- text
import Data.Text (Text, pack, unpack)

import AirportGroups as AG
import Groups

--------------------------------------------------------------------------------


prog :: Parsec String AirportMaps (Program Airport)
prog = do
--  spaces
  s <- stmt
  inp <- getInput
  let more = not $ null inp
  if more
  then do
    ss <- prog
    return (s:ss)
  else
   return [s]

{-
  when more $ do
    ss <- prog
    return $ s:ss
-}

{-
prog = do
  s1 <- stmt
  s2 <- stmt
  return [s1, s2]
-}

--prog = many stmt
--prog = many1 stmt
--prog = semiSep1 stmt

stmt :: Parsec String AirportMaps (Stmt Airport)
stmt = do
  inp <- getInput
  traceM $ "stmt: inp: " ++ inp
  spaces
  st <- stmtTxt
--  spaces
--  void $ char ';'
  spaces
  return st


stmtTxt :: Parsec String AirportMaps (Stmt Airport)
stmtTxt = 
  try predAssignStmt
--    setAssignStmt
   <|> try setAssignStmt
--   <|> try predAssignStmt
   <|> try printStmt
--  <|> try elemOfStmt


setAssignStmt :: Parsec String AirportMaps (Stmt Airport)
setAssignStmt = do
  inp <- getInput
  traceM $ "setAssignStmt: inp: " ++ inp
  var <- setVar
  spaces
  void $ char '='
  spaces
  expr <- setExpr
  spaces
  void $ char ';'
  return $ AssignSet var expr


predAssignStmt :: Parsec String AirportMaps (Stmt Airport)
predAssignStmt = do
  --inp <- getInput
  --traceM $ "predAssignStmt: inp: " ++ inp
  var <- predVar
  spaces
  void $ char '='
  spaces
  expr <- predExpr
  spaces
  void $ char ';'
  return $ AssignPred var expr


printStmt :: Parsec String st (Stmt Airport)
printStmt = do
  void $ string "print("
  spaces
  var <- setVar
  spaces
  void $ char ')'
  spaces
  void $ char ';'
  return $ Print var
  
elemOfStmt :: Parsec String st (Stmt Airport)
elemOfStmt = error "elemOfStmt"

setVar :: Parsec String st Var
setVar = pack <$> identifier

setExpr :: Parsec String AirportMaps (SetExpr Airport)
setExpr = do
     inp <- getInput
     traceM $ "setExpr: inp: " ++ inp
--  <|> try setParensExpr
     try setUnionExpr
    <|> try setIntersectionExpr
--  <|> try setDifferenceExpr
    <|> try setSuchThatExpr
    <|> try setVarExpr
    <|> try setAirportsExpr

setOpArgExpr :: Parsec String AirportMaps (SetExpr Airport)
setOpArgExpr =
  try setVarExpr
  <|> try setAirportsExpr

setVarExpr :: Parsec String AirportMaps (SetExpr Airport)
setVarExpr = SVar <$> setVar

setExprCode :: Parsec String st (SetExpr AirportCode)
setExprCode =
  try (SVar <$> setVar)
  <|> try ((Elems . fromList) <$> airportIdentifierList)

setAirportsExpr :: Parsec String AirportMaps (SetExpr Airport)
setAirportsExpr = (Elems . fromList) <$> airports

setParensExpr :: Parsec String AirportMaps (SetExpr Airport)
setParensExpr = do
  void $ char '('
  spaces
  s <- setExpr
  spaces
  void $ char ')'
  return s

setUnionExpr :: Parsec String AirportMaps (SetExpr Airport)
setUnionExpr = do
--  s1 <- setExpr
  s1 <- setOpArgExpr
  spaces
  union
  spaces
--  s2 <- setExpr
  s2 <- setOpArgExpr
  return $ Union s1 s2

union :: Parsec String AirportMaps ()
union = void $ char '+'

setIntersectionExpr :: Parsec String AirportMaps (SetExpr Airport)
setIntersectionExpr = do
  s1 <- setOpArgExpr
  spaces
  intersection
  spaces
  s2 <- setOpArgExpr
  return $ Intersection s1 s2

intersection :: Parsec String AirportMaps ()
intersection = void $ char '^'

setDifferenceExpr :: Parsec String AirportMaps (SetExpr Airport)
setDifferenceExpr = do
  s1 <- setExpr
  spaces
  difference
  spaces
  s2 <- setExpr
  return $ Difference s1 s2

difference :: Parsec String AirportMaps ()
difference = void $ char '-'

setSuchThatExpr :: Parsec String AirportMaps (SetExpr Airport)
setSuchThatExpr = do
  --inp <- getInput
  --traceM $ "setSuchThatExpr: inp: " ++ inp
  s <- setOpArgExpr
  spaces
  void $ char '|'
  spaces
  p <- predOpArgExpr
  return $ SuchThat s p


airportIdentifiers :: Parsec String st [AirportCode]
airportIdentifiers = commaSep1 airportIdentifier

airportIdentifierList :: Parsec String st [AirportCode]
airportIdentifierList = squares airportIdentifiers

airports' :: Parsec String AirportMaps [Airport]
airports' = commaSep1 airport

airports :: Parsec String AirportMaps [Airport]
airports = squares airports'

airport :: Parsec String AirportMaps Airport
airport = do
  code <- airportIdentifier
  maps <- getState
  
  let mAirport = AG.lookup code maps

  case mAirport of
    Nothing -> unexpected "unknown airport"
    Just airport -> return airport


airportIdentifier :: Parsec String st AirportCode
airportIdentifier = do
  inp <- getInput
  traceM $ "airportIdentifier: inp: " ++ inp
  try (do
    faa <- faaIdentifier
    return $ FAAac faa)
  <|> try (do
    icao <- icaoIdentifier
    return $ ICAOac icao)
  <|> try (do
    iata <- iataIdentifier
    return $ IATAac iata)


faaIdentifier :: Parsec String st FAA
faaIdentifier = do
  inp <- getInput
  traceM $ "faaIdentifier: inp: " ++ inp
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


predExpr ::  Parsec String AirportMaps (PredExpr Airport)
predExpr =
  try predAndExpr
  <|> try predOrExpr
  <|> try predNotExpr
  <|> try isInCountryPL
  <|> try predVarExpr



predVarExpr :: Parsec String AirportMaps (PredExpr Airport)
predVarExpr = PVar <$> predVar

predAndExpr :: Parsec String AirportMaps (PredExpr Airport)
predAndExpr = do
  p <- predOpArgExpr
  spaces
  void $ string "&&"
  spaces
  q <- predOpArgExpr
  return $ POr p q


predOrExpr :: Parsec String AirportMaps (PredExpr Airport)
predOrExpr = do
  p <- predOpArgExpr
  spaces
  void $ string "||"
  spaces
  q <- predOpArgExpr
  return $ POr p q

predNotExpr :: Parsec String AirportMaps (PredExpr Airport)
predNotExpr = do
  inp <- getInput
  traceM $ "predNotExpr: inp: " ++ inp
  void $ char '!'
  spaces
  p <- predOpArgExpr
  return $ PNot p

predOpArgExpr :: Parsec String AirportMaps (PredExpr Airport)
predOpArgExpr = do
  inp <- getInput
  traceM $ "predOpArgExpr: inp: " ++ inp
  try isInCountryPL
    <|> try predNotExpr
    <|> try predVarExpr



predVar :: Parsec String st Var
predVar = pack <$> identifier

isInCountryOld :: Parsec String st (PredExpr Airport)
isInCountryOld = do
  void $ string "isInCountry"
  void $ char '('
  spaces
  code <- many1 letter
  spaces
  void $ char ')'
  return $ PLit $ Pred $ isInCountry $ pack code

predLit :: String -> (Text -> Airport -> Bool)
        -> Parsec String st (PredExpr Airport)
predLit predFtnName predFtn = do
  void $ string predFtnName
  void $ char '('
  spaces
  code <- many1 letter
  spaces
  void $ char ')'
  return $ PLit $ Pred $ predFtn $ pack code 

isInCountryPL :: Parsec String st (PredExpr Airport)
isInCountryPL  = predLit "isInCountry" isInCountry

isInStatePL :: Parsec String st (PredExpr Airport)
isInStatePL  = predLit "isInState" isInState

isNorthOfPL :: Parsec String st (PredExpr Airport)
isNorthOfPL = do
  void $ string "isNorthOfLatitudeDegs("
  spaces
  lat <- float
  spaces
  void $ char ')'
  return $ PLit $ Pred $ isNorthOf lat

spaces :: Parsec String st ()
spaces = skipMany space

lexer = P.makeTokenParser haskellDef
commaSep1 = P.commaSep1 lexer
squares = P.squares lexer
identifier = P.identifier lexer
semiSep1 = P.semiSep1 lexer
float = P.float lexer


--------------------------------------------------------------------------------

loadAirports :: FilePath -> IO AirportMaps
loadAirports fp = do
  lns <- lines <$> readFile fp
  let idMap = foldr insertAirport IM.empty $ tail lns
  return $ mkAirportMaps idMap

  
  
  
insertAirport :: String -> AirportIdMap -> AirportIdMap
insertAirport ln map0 = IM.insert iD ap map0
  where
    [iDStr, faaCode, icaoCode, iataCode, latDegsStr
      , lonDegsStr, countryCode, stateCodeStr] = words ln
      
    iD = read iDStr
    latDegs = read latDegsStr
    lonDegs = read lonDegsStr
    mFaa  = fromNullStr faaCode
    mIcao = fromNullStr icaoCode
    mIata = fromNullStr iataCode
    
    codes = AirportCodes (FAA  <$> fromNullStr faaCode)
                         (ICAO <$> fromNullStr icaoCode)
                         (IATA <$> fromNullStr iataCode)
                         Nothing

    mStateCode = readUsStateCode <$> fromNullStr stateCodeStr

    ap = Airport (ID iD) codes (pack countryCode) mStateCode
                 (LatLonDeg latDegs lonDegs)



fromNullStr :: String -> Maybe Text
fromNullStr str0 = case str0 of
  "NULL" -> Nothing
  str -> Just $ pack str
    
--------------------------------------------------------------------------------

test1 :: IO ()
test1 = do
  parseTest faaIdentifier "FAA:SFO"
  parseTest icaoIdentifier "ICAO:KSFO"
  parseTest airportIdentifier "FAA:SFO"
  parseTest airportIdentifier "ICAO:KSFO"
  parseTest airportIdentifier "IATA:YSFO"
  parseTest airportIdentifiers {- $ stripSpaces -} "ICAO:KSFO, FAA:LAX"
  parseTest airportIdentifierList {- $ stripSpaces -} "[ICAO:KSFO, FAA:LAX]"
  parseTest setVar "s1_"
  parseTest setExprCode "s1_"
  parseTest setExprCode {- $ stripSpaces -} "[ICAO:KSFO]"
  parseTest setExprCode {- $ stripSpaces -} "[ICAO:KSFO, FAA:LAX]"
  --parseTest setAssignStmt $ stripSpaces "s1 = [ICAO:KSFO, FAA:LAX]"
  --parseTest prog $ stripSpaces "s1 = [ICAO:KSFO, FAA:LAX] ; s2 = [IATA:XYZ]"

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
    pStr1 = {- stripSpaces -} "s1 = [ICAO:KSFO]; print(s1)" --  ; s2 = [ICAO:KMDW]"
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

