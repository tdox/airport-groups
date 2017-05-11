{-# LANGUAGE OverloadedStrings #-}

module Parser where


-- base
import Control.Monad (foldM, forM_, void)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe, maybe)
import Debug.Trace

-- containers
import Data.Set (fromList)
import qualified Data.IntMap as IM
import qualified Data.Map as M

-- parsec
import Text.Parsec (Parsec, ParseError, (<|>), (<?>), char, getState, letter, many1, parse, parseTest, runParser, skipMany, space, string, try, unexpected)

import qualified Text.Parsec.Token as P -- (commaSep1, makeTokenParser)
import Text.Parsec.Language (haskellDef)

-- text
import Data.Text (Text, pack, unpack)

import AirportGroups as AG
import Groups

--------------------------------------------------------------------------------
-- note: change AirportCode to Airport

prog :: Parsec String AirportMaps (Program Airport)
prog = semiSep1 stmt

stmt :: Parsec String AirportMaps (Stmt Airport)
stmt =
  try setAssignStmt
--  <|> try predAssignStmt
  <|> try printStmt
  <|> try elemOfStmt


setAssignStmt :: Parsec String AirportMaps (Stmt Airport)
setAssignStmt = do
  var <- setVar
  spaces
  void $ char '='
  spaces
  expr <- setExpr
  return $ AssignSet var expr


predAssignStmt :: Parsec String AirportMaps (Stmt Airport)
predAssignStmt = error "predAssignStmt"


printStmt :: Parsec String st (Stmt Airport)
printStmt = do
  void $ string "print("
  spaces
  var <- setVar
  spaces
  void $ char ')'
  return $ Print var
  
elemOfStmt :: Parsec String st (Stmt Airport)
elemOfStmt = error "elemOfStmt"

setVar :: Parsec String st Var
setVar = pack <$> identifier

setExpr :: Parsec String AirportMaps (SetExpr Airport)
setExpr =
  try (SVar <$> setVar)
  <|> try setParensExpr
  <|> try ((Elems . fromList) <$> airports)
  <|> try setUnionExpr
  <|> try setIntersectionExpr
  <|> try setDifferenceExpr
  <|> try setSuchThatExpr

setExprCode :: Parsec String st (SetExpr AirportCode)
setExprCode =
  try (SVar <$> setVar)
  <|> try ((Elems . fromList) <$> airportIdentifierList)


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
  s1 <- setExpr
  spaces
  union
  spaces
  s2 <- setExpr
  return $ Union s1 s2

union :: Parsec String AirportMaps ()
union = void $ char '+'

setIntersectionExpr :: Parsec String AirportMaps (SetExpr Airport)
setIntersectionExpr = do
  s1 <- setExpr
  spaces
  intersection
  spaces
  s2 <- setExpr
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
setSuchThatExpr = error "setSuchThatExpr"


suchThat :: Parsec String AirportMaps ()
suchThat = void $ char '|'


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


predExpr ::  Parsec String st (PredExpr Airport)
predExpr =
  try (PVar <$> predVar)
  <|> try isInCountryPL


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



--stripSpaces :: String -> String
--stripSpaces str = filter (not . isSpace) str


  



-- predAssignStmt ::


  


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
    -- iD = read $ {- traceShow iDStr -} iDStr
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






{-
execProg :: Program Airport -> Either Err (Output, Store Airport)
execProg p = foldM execStmt ("", M.empty) p
-}

{-
execSt :: AirportMaps -> (Output, Store Airport) -> Stmt Airport
       -> Either Err (Output, Store Airport)
       
execSt aps (out0, st0) stmt =  execStmt stmt (out0, st0)
-}
 


--------------------------------------------------------------------------------
--convertSetExpr :: AirportMaps -> SetExpr AirportCode -> SetExpr Airport
--convertSetExpr aps setExprAC = undefined

