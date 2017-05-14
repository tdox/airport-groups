{-# LANGUAGE OverloadedStrings #-}

-- TODO 5/13/17 rename to Airport
module AirportGroups where

-- base
import Control.Applicative ((<|>))
import Data.Maybe (fromJust, isJust)

-- containers
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)
import qualified Data.Map as M

-- text
import Data.Text (Text, unpack)
import qualified Data.Text as T

--------------------------------------------------------------------------------

type CountryCode = Text
type UsStateCode = (Char, Char)

data ICAO = ICAO Text deriving (Eq, Ord)
data IATA = IATA Text deriving (Eq, Ord)
data FAA  = FAA  Text deriving (Eq, Ord)

instance Show ICAO where
  show (ICAO t) = "ICAO:" ++ unpack t

instance Show IATA where
  show (IATA t) = "IATA:" ++ unpack t

instance Show FAA where
  show (FAA t) = "FAA:" ++ unpack t

data CountryAirportCode = CAC CountryCode Text
                           deriving (Eq, Ord, Show)


data AirportCode = ICAOac ICAO
                 | IATAac IATA
                 | FAAac  FAA
                 | CACac CountryAirportCode
                 deriving (Eq, Ord, Show)


data AirportCodes = AirportCodes { acFAA  :: Maybe FAA
                                 , acICAO :: Maybe ICAO
                                 , acIATA :: Maybe IATA
                                 , acCAC  :: Maybe CountryAirportCode
                                 } deriving (Eq, Ord, Show)

-- data AirportCountryCode = AirportCountryCode CountryCode AirporCode
-- examples "ICAO:KSFO" "CAC:USA/SFO"

data ID a = ID {iD :: Int} deriving (Eq, Ord, Show)

data LatLonDeg = LatLonDeg { lat :: Double
                           , lon :: Double
                           } deriving (Eq, Ord, Show)


data Airport = Airport { apId          :: ID Airport
                       , apCodes       :: AirportCodes
                       , apCountryCode :: CountryCode
                       , apUsStateCode :: Maybe (UsStateCode)
                       , apLatLon      :: LatLonDeg
                       } deriving (Eq, Ord)

instance (Show Airport) where
  show ap = showAirportCodes $ apCodes ap


type AirportIdMap = IntMap Airport
type AirportFaaMap = Map FAA Airport
type AirportIcaoMap = Map ICAO Airport

data AirportMaps = AirportMaps { apIdMap   :: AirportIdMap
                               , apFaaMap  :: AirportFaaMap
                               , apIcaoMap :: AirportIcaoMap
                               }

--------------------------------------------------------------------------------



showAirportCodes :: AirportCodes -> String
showAirportCodes codes = case mStr of
  Just str -> str
  Nothing -> error "showAirportCodes"
  where
    mStr :: Maybe String
    mStr =  (show <$> (acFAA codes))
        <|> (show <$> (acICAO codes))
        <|> (show <$> (acIATA codes))
                          


showMaybe :: Show a => Maybe a -> Maybe String
showMaybe mx = show <$> mx


lookup :: AirportCode -> AirportMaps -> Maybe Airport
lookup code airports =

  case code of
    ICAOac icao -> M.lookup icao $ apIcaoMap airports
    FAAac  faa  -> M.lookup faa  $ apFaaMap airports
    IATAac _    -> error "TODO AirportGroups.lookup: not yet implemented"
    CACac  _    -> error "TODO AirportGroups.lookup: not yet implemented"



mkFaaMap :: AirportIdMap -> AirportFaaMap
mkFaaMap idMap = foldr (\(_, ap) -> case acFAA (apCodes ap) of
                               Nothing -> id
                               Just faa -> M.insert faa ap)
                        M.empty
                        (IM.toList idMap)

mkIcaoMap :: AirportIdMap -> AirportIcaoMap
mkIcaoMap idMap = foldr (\(_, ap) -> case acICAO (apCodes ap) of
                               Nothing -> id
                               Just icao -> M.insert icao ap)
                        M.empty
                        (IM.toList idMap)

mkAirportMaps :: AirportIdMap -> AirportMaps
mkAirportMaps idMap = AirportMaps idMap (mkFaaMap idMap) (mkIcaoMap idMap)


isInCountry :: Text -> Airport -> Bool
isInCountry countryCode ap = apCountryCode ap == countryCode

isInState :: Text -> Airport -> Bool
isInState stateCode ap =
  apCountryCode ap == "USA"
  && isJust mStateCode
  && fromJust mStateCode == readUsStateCode stateCode
  where
    mStateCode = apUsStateCode ap

isNorthOf :: Double -> Airport -> Bool
isNorthOf lati ap = (lat . apLatLon) ap >= lati

isSouthOf :: Double -> Airport -> Bool
isSouthOf lati ap = (lat . apLatLon) ap <= lati



--------------------------------------------------------------------------------
{-

<program> ::=  <stmt> ";"  | <stmt> ";" <program>

<stmt> ::= <set-assign-stmt>
         | <pred-assign-stmt>
         | <print-stmt>
         | <elem-of-stmt>

<set-assign-stmt> ::= <set-var> "=" <set-expr>
<pred-assign-stmt> ::= <pred-var> "=" <pred-expr>
<print-stmt> ::= "print(" <set-var> ")"
<elem-of-stmt> ::= "elem(" <airport-identifier> "," <set-expr> ")"

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

readUsStateCode :: Text -> UsStateCode
readUsStateCode txt
  | T.length txt /= 2 = error "readUsStateCode: must be 2 chars"
  | otherwise = (T.head txt, T.last txt)

--------------------------------------------------------------------------------

a1 = Airport (ID 1)
             (AirportCodes (Just (FAA "SFO")) Nothing Nothing Nothing)
             "USA"
             (Just ('C', 'A'))
             (LatLonDeg 33.7749 (-122.4194))
  
a2 = Airport (ID 2)
             (AirportCodes (Just (FAA "LAX")) Nothing Nothing  (Just (CAC "USA" "LAX")))
             "USA"
             (Just ('C', 'A'))
             (LatLonDeg 33.9416 (-118.4085))
             
