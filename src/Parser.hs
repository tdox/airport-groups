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
import Text.Parsec (Parsec, ParseError, (<|>), (<?>), char, getState, letter, many1, parse, parseTest, runParser, string, try, unexpected)

import qualified Text.Parsec.Token as P -- (commaSep1, makeTokenParser)
import Text.Parsec.Language (haskellDef)

-- text
import Data.Text (Text, pack, unpack)

import AirportGroups as AG
import Groups

--------------------------------------------------------------------------------
-- note: change AirportCode to Airport

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

airport :: Parsec String AirportMaps Airport
airport = do
  code <- airportIdentifier
  maps <- getState
  
  let mAirport = AG.lookup code maps

  case mAirport of
    Nothing -> unexpected "known airport"
    Just airport -> return airport
    

commaSep1 = P.commaSep1 lexer
squares = P.squares lexer
identifier = P.identifier lexer
semiSep1 = P.semiSep1 lexer

airportIdentifiers :: Parsec String st [AirportCode]
airportIdentifiers = commaSep1 airportIdentifier

airportIdentifierList :: Parsec String st [AirportCode]
airportIdentifierList = squares airportIdentifiers

airports' :: Parsec String AirportMaps [Airport]
airports' = commaSep1 airport

airports :: Parsec String AirportMaps [Airport]
airports = squares airports'


stripSpaces :: String -> String
stripSpaces str = filter (not . isSpace) str

setVar :: Parsec String st Var
setVar = pack <$> identifier


setExprCode :: Parsec String st (SetExpr AirportCode)
setExprCode =
  try (SVar <$> setVar)
  <|> try ((Elems . fromList) <$> airportIdentifierList)

  
setExpr :: Parsec String AirportMaps (SetExpr Airport)
setExpr =
  try (SVar <$> setVar)
  <|> try ((Elems . fromList) <$> airports)


setAssignStmt :: Parsec String AirportMaps (Stmt Airport)
setAssignStmt = do
  var <- setVar
  void $ char '='
  expr <- setExpr
  return $ AssignSet var expr

-- predAssignStmt ::

printStmt :: Parsec String st (Stmt Airport)
printStmt = do
  void $ string "print("
  var <- setVar
  void $ char ')'
  return $ Print var
  

stmt :: Parsec String AirportMaps (Stmt Airport)
stmt =
  try (setAssignStmt)
  <|> try (printStmt)

  

prog :: Parsec String AirportMaps (Program Airport)
prog = semiSep1 stmt

--------------------------------------------------------------------------------

loadAirports :: FilePath -> IO AirportMaps
loadAirports fp = do
  lns <- lines <$> readFile fp
  let idMap = foldr insertAirport IM.empty $ tail lns
  return $ mkAirportMaps idMap

  
  
  
insertAirport :: String -> AirportIdMap -> AirportIdMap
insertAirport ln map0 = IM.insert iD ap map0
  where
    [iDStr, faaCode, icaoCode, iataCode, latDegsStr, lonDegsStr] = words ln
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

    ap = Airport (ID iD) codes "USA" (Just (readUsStateCode "CA"))
                 (latDegs, lonDegs)



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
  parseTest airportIdentifiers $ stripSpaces "ICAO:KSFO , FAA:LAX"
  parseTest airportIdentifierList $ stripSpaces "[  ICAO:KSFO , FAA:LAX  ]"
  parseTest setVar "s1_"
  parseTest setExprCode "s1_"
  parseTest setExprCode $ stripSpaces "[ ICAO : KSFO ]"
  parseTest setExprCode $ stripSpaces "[ ICAO : KSFO, FAA:LAX ]"
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
    pStr1 = stripSpaces "s1 = [ICAO:KSFO]; print(s1)" --  ; s2 = [ICAO:KMDW]"
    fp = "../misc/airports_dev.txt"
    
  aps <- loadAirports fp

  let
    ep1 :: Either ParseError (Program Airport)
    ep1 = runParser prog aps "dummySrc" pStr1

--  print ep1

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
convertSetExpr :: AirportMaps -> SetExpr AirportCode -> SetExpr Airport
convertSetExpr aps setExprAC = undefined

