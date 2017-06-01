-- {-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where


-- base
import Control.Monad (void)
-- import Debug.Trace

-- containers
import Data.Set (fromList)
import qualified Data.IntMap as IM

-- parsec
import Text.Parsec (Parsec, (<|>), char, getInput
                   , getState, letter, many1
                   , skipMany, space, string, try, unexpected)

import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

-- text
import Data.Text (Text, pack)

-- airport
import Airport (Airport(Airport), AirportCode(FAAac, ICAOac, IATAac)
               , AirportCode, AirportCodes(AirportCodes), AirportIdMap
               , AirportMaps, FAA(FAA)
               , ID(ID), IATA(IATA), ICAO(ICAO)
               , LatLonDeg(LatLonDeg)
               , isEastOf, isInCountry, isInState, isNearAirport
               , isNorthOf
               , isSouthOf
               , isWestOf, lookup
               , mkAirportMaps, readUsStateCode)
       
import Groups (Program, Pred(Pred)
              , PredExpr(PAnd, PLit, PNot, POr, PParens, PVar)
              , SetExpr(Difference, Elems, Intersection, SParens, SVar
                       , SuchThat, Union)
              , Stmt(AssignPred, AssignSet, ElemOf, Print), Var
              )

--------------------------------------------------------------------------------
{-

<program> ::=  <stmt> ";"  | <stmt> ";" <program>

<stmt> ::= <set-assign-stmt>
         | <pred-assign-stmt>
         | <print-stmt>
         | <airport-is-in-set-stmt>
         | <airport-satisfies-pred-stmt>

<set-assign-stmt> ::= <set-var> "=" <set-expr>
<pred-assign-stmt> ::= <pred-var> "=" <pred-expr>
<print-stmt> ::= "print(" <set-var> ")"
<airport-is-in-set-stmt> ::= "airportIsInSet(" <airport-identifier> "," <set-var> ")"
<airport-satisfies-pred-stmt> ::= "airportSatisfiesPredicate(" <airport-identifier> "," <pred-var> ")

<set-var> ::= <alphanumeric>

<set-expr> ::= <set-var>
             | <set-paren-expr>
             | <airport-identifier-list>
             | <set-union-expr>
             | <set-intersection-expr>
             | <set-difference-expr>
             | <set-such-that-expr>

<set-paren-expr> ::= "(" <set-expr> ")"
<set-union-expr> ::= <set-op-arg-expr> <union> <set-op-arg-expr>

<set-op-arg-expr> :: = <set-var>
                     | <set-paren-expr>
                     | <set-airport-identifier-list>

<set-intersection-expr> ::= <set-expr> <intersection> <set-expr>
<set-such-that-expr> ::=  <set-expr> "|" <pred-expr>


<union> ::=  "+"
<intersection> ::=  "^"

<airport-identifer-list> = "[" <airport-identifiers> "]"

<airport-identifiers> ::= <airport-identifier>
                        | <airport-identifier> "," <airport-identifiers>

<airport-identifier> ::= <faa-identifier>
                       | <icao-identifier>
                       | <iata-identifier>
                       | <acc-identifier>

<faa-identifier>  ::= "FAA:"<characters>
<icao-identifier> ::= "ICAO:"<characters>
<iata-identifier> ::= "IATA:"<characters>
<acc-identifier>  ::= "ACC:"<country-code>":"<characters>

<pred-expr> ::= <pred-var>
              | "(" <pred-expr> ")"
              | <pred-op-arg-expr> <pred-op> <pred-op-arg-expr>
              | <pred-not-expr> <pred-op-arg-expr>
              | "isInState(" <state-code> ")"
              | "isInCountry(" <country-code> ")"
              | "isNearInMiles(" <airport-identifier> "," <float> ")"
              | "isNorthOfLatitudeDegs(" <float> ")"
              | "isSouthOfLatitudeDegs(" <float> ")"
              | "isSouthOfLatitudeDegs(" <float> ")"
              | "isBetweenLongitudesDegs(" <float> "," <float> ")"

<pred-op> ::= <and> | <or>
<and> ::= "&&"
<or> ::= "||"
<not> ::= "!"

<state-code" ::= <character><character>
<country-code" ::= <characters>

-}
--------------------------------------------------------------------------------

prog :: Parsec String AirportMaps (Program Airport)
--prog = endBy1 stmt (char ';') 

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
--  inp <- getInput
--  traceM $ "stmt: inp: " ++ inp
  spaces
  st <- stmtTxt
--  spaces
--  void $ char ';'
  spaces
  return st


stmtTxt :: Parsec String AirportMaps (Stmt Airport)
stmtTxt = 
  try predAssignStmt
   <|> try setAssignStmt
   <|> try printStmt
   <|> try airportIsInSetStmt
--   <|> try airportSatisfiesPredStmt


setAssignStmt :: Parsec String AirportMaps (Stmt Airport)
setAssignStmt = do
--  inp <- getInput
--  traceM $ "setAssignStmt: inp: " ++ inp
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
  
airportIsInSetStmt :: Parsec String AirportMaps (Stmt Airport)
airportIsInSetStmt = do
  void $ string "airportIsInSet("
  spaces
  ap <- airport
  spaces
  void $ char ','
  spaces
  setV <- setVarExpr
  spaces
  void $ char ')'
  spaces
  void $ char ';'
  return $ ElemOf ap setV

{-
airportSatisfiesPredStmt :: Parsec String AirportMaps (Stmt Airport)
airportSatisfiesPredStmt = do
  void $ string "airportSatisfiesPredicate("
  spaces
  ap <- airport
  spaces
  void $ char ','
  spaces
  predV <- predVarExpr
  spaces
  void $ char ')'
  spaces
  void $ char ';'
  return $ AirportSatisfiesPred ap predV
-}

setVar :: Parsec String st Var
setVar = pack <$> identifier

setExpr :: Parsec String AirportMaps (SetExpr Airport)
setExpr = do
--     inp <- getInput
--     traceM $ "setExpr: inp: " ++ inp
     try setUnionExpr
      <|> try setIntersectionExpr
      <|> try setDifferenceExpr
      <|> try setSuchThatExpr
      <|> try setVarExpr
      <|> try setAirportsExpr
      <|> try setParensExpr


setOpArgExpr :: Parsec String AirportMaps (SetExpr Airport)
setOpArgExpr = do
--  inp <- getInput
--  traceM $ "setOpArgExpr: inp: " ++ inp
  try setVarExpr
  <|> try setAirportsExpr
  <|> try setParensExpr

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
--  inp <-getInput
--  traceM $ "setParensExpr: " ++ inp
  se <- parens setExpr
  return $ SParens se
  

setUnionExpr :: Parsec String AirportMaps (SetExpr Airport)
setUnionExpr = do
  s1 <- setOpArgExpr
  spaces
  union
  spaces
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
  s1 <- setOpArgExpr
  spaces
  difference
  spaces
  s2 <- setOpArgExpr
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
  p <- suchThatPredArgExpr
  return $ SuchThat s p


airportIdentifiers :: Parsec String st [AirportCode]
airportIdentifiers = commaSep1 airportIdentifier

airportIdentifierList :: Parsec String st [AirportCode]
airportIdentifierList = braces airportIdentifiers

airports' :: Parsec String AirportMaps [Airport]
airports' = commaSep1 airport

airports :: Parsec String AirportMaps [Airport]
airports = braces airports'

airport :: Parsec String AirportMaps Airport
airport = do
  code <- airportIdentifier
  maps <- getState
  
  let mAirport = Airport.lookup code maps

  case mAirport of
    Nothing -> unexpected "unknown airport"
    Just ap -> return ap


--set :: Parsec String AirportMaps (SetVar Airport)
--set = do
  

airportIdentifier :: Parsec String st AirportCode
airportIdentifier = do
--  inp <- getInput
--  traceM $ "airportIdentifier: inp: " ++ inp
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
--  inp <- getInput
--  traceM $ "faaIdentifier: inp: " ++ inp
  void $ string "FAA:"
  code <- many1 letter
  return $ FAA $ pack code

icaoIdentifier :: Parsec String st ICAO
icaoIdentifier = do
  void $ string "ICAO:"
  code <- many1 letter
  return $ ICAO $ pack code

iataIdentifier :: Parsec String st IATA
iataIdentifier = do
  void $ string "IATA:"
  code <- many1 letter
  return $ IATA $ pack code


predExpr ::  Parsec String AirportMaps (PredExpr Airport)
predExpr =
  try predAndExpr
  <|> try predOrExpr
  <|> try predNotExpr
  <|> try isInCountryPL
  <|> try isInStatePL
  <|> try isNorthOfPL
  <|> try isSouthOfPL
  <|> try isEastOfPL
  <|> try isWestOfPL
  <|> try isNearAirportPL
  <|> try predVarExpr
  <|> try predParensExpr



predVarExpr :: Parsec String AirportMaps (PredExpr Airport)
predVarExpr = PVar <$> predVar

predAndExpr :: Parsec String AirportMaps (PredExpr Airport)
predAndExpr = do
  p <- predOpArgExpr
  spaces
  void $ string "&&"
  spaces
  q <- predOpArgExpr
  return $ PAnd p q


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
--  inp <- getInput
--  traceM $ "predNotExpr: inp: " ++ inp
  void $ char '!'
  spaces
  p <- predOpArgExpr
  return $ PNot p

suchThatPredArgExpr :: Parsec String AirportMaps (PredExpr Airport)
suchThatPredArgExpr = predExpr

predOpArgExpr :: Parsec String AirportMaps (PredExpr Airport)
predOpArgExpr = do
--  inp <- getInput
--  traceM $ "predOpArgExpr: inp: " ++ inp
  try isInCountryPL
    <|> try isInStatePL
    <|> try isNorthOfPL
    <|> try isSouthOfPL
    <|> try isEastOfPL
    <|> try isWestOfPL
    <|> try isNearAirportPL
    <|> try predNotExpr
    <|> try predVarExpr
    <|> try predParensExpr

predParensExpr :: Parsec String AirportMaps (PredExpr Airport)
predParensExpr = do
--  inp <-getInput
--  traceM $ "predParensExpr: " ++ inp
  ex <- parens predExpr
  return $ PParens ex


predVar :: Parsec String st Var
predVar = pack <$> identifier


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
--  inp <- getInput
--  traceM $ "isNorthOfPL: " ++ inp
  void $ string "isNorthOf("
  spaces
  lat <- intOrFloat
  spaces
  void $ char ')'
  return $ PLit $ Pred $ isNorthOf lat

isSouthOfPL :: Parsec String st (PredExpr Airport)
isSouthOfPL = do
--  inp <- getInput
--  traceM $ "isSouthOfPL: " ++ inp
  void $ string "isSouthOf("
  spaces
  lat <- intOrFloat
  spaces
  void $ char ')'
  return $ PLit $ Pred $ isSouthOf lat

isEastOfPL :: Parsec String st (PredExpr Airport)
isEastOfPL = do
--  inp <- getInput
--  traceM $ "isEastOfPL: " ++ inp
  void $ string "isEastOf("
  spaces
  lon <- intOrFloat
  spaces
  void $ char ')'
  return $ PLit $ Pred $ isEastOf lon

isWestOfPL :: Parsec String st (PredExpr Airport)
isWestOfPL = do
--  inp <- getInput
--  traceM $ "isWestOfPL: " ++ inp
  void $ string "isWestOf("
  spaces
  lon <- intOrFloat
  spaces
  void $ char ')'
  return $ PLit $ Pred $ isWestOf lon

isNearAirportPL :: Parsec String AirportMaps (PredExpr Airport)
isNearAirportPL = do
--  inp <- getInput
--  traceM $ "isNearAirportPL: " ++ inp
  void $ string "isNearAirport("
  spaces
  ap <- airport
  spaces
  void $ char ','
  spaces
  rad <-intOrFloat
  spaces
  void $ char ')'
  return $PLit $ Pred $ isNearAirport ap rad

intOrFloat :: Parsec String st Double
intOrFloat = do
  try (do
    f <- float
    return f)
  <|> try (do
    i <- integer
    return $ fromInteger i
    )

    

spaces :: Parsec String st ()
spaces = skipMany space

lexer :: P.TokenParser st
lexer = P.makeTokenParser haskellDef

commaSep1 = P.commaSep1 lexer
braces = P.braces lexer
identifier = P.identifier lexer
semiSep1 = P.semiSep1 lexer
float = P.float lexer
parens = P.parens lexer
integer = P.integer lexer


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
