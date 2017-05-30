{-# LANGUAGE OverloadedStrings #-}

module Airport where

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
type AirportIataMap = Map IATA Airport

data AirportMaps = AirportMaps { apIdMap   :: AirportIdMap
                               , apFaaMap  :: AirportFaaMap
                               , apIcaoMap :: AirportIcaoMap
                               , apIataMap :: AirportIataMap
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
    IATAac iata -> M.lookup iata $ apIataMap airports
    CACac  _    -> error "TODO Airport.lookup: not yet implemented"



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
                        
mkIataMap :: AirportIdMap -> AirportIataMap
mkIataMap idMap = foldr (\(_, ap) -> case acIATA (apCodes ap) of
                               Nothing -> id
                               Just icao -> M.insert icao ap)
                        M.empty
                        (IM.toList idMap)

mkAirportMaps :: AirportIdMap -> AirportMaps
mkAirportMaps idMap =
  AirportMaps idMap
              (mkFaaMap idMap) (mkIcaoMap idMap) (mkIataMap idMap)


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
isNorthOf lati ap = (lat . apLatLon) ap > lati

isSouthOf :: Double -> Airport -> Bool
isSouthOf lati ap = (lat . apLatLon) ap < lati

isEastOf :: Double -> Airport -> Bool
isEastOf long ap = (lon . apLatLon) ap > long

isWestOf :: Double -> Airport -> Bool
isWestOf long ap = (lon . apLatLon) ap < long

isNearAirport :: Airport -> Double -> Airport -> Bool
isNearAirport ap0 radiusM ap1 =
    distanceMiles (apLatLon ap0) (apLatLon ap1) <= radiusM
    


degreesToRadians :: Double -> Double
degreesToRadians deg = deg * pi / 180.0

distanceMiles :: LatLonDeg -> LatLonDeg -> Double
distanceMiles (LatLonDeg p1Deg l1Deg)
              (LatLonDeg p2Deg l2Deg) = d
  where
    p1 = degreesToRadians p1Deg
    l1 = degreesToRadians l1Deg
    
    p2 = degreesToRadians p2Deg
    l2 = degreesToRadians l2Deg

    halfDeltaP = (p2 - p1) / 2.0
    halfDeltaL = (l2 - l1) / 2.0

    sinHalfDeltaP = sin(halfDeltaP)
    sinHalfDeltaL = sin(halfDeltaL)

    a = sinHalfDeltaP * sinHalfDeltaP
      + cos p1 * cos p2 * sinHalfDeltaL * sinHalfDeltaL

    c = 2.0 * atan2 (sqrt a) (sqrt (1.0 - a))

    earthRad = 3959 -- miles

    d = c * earthRad
    

distanceBetweenAirports :: Airport -> Airport -> Double
distanceBetweenAirports ap1 ap2 = distanceMiles latLon1 latLon2
  where
    [latLon1, latLon2] = map apLatLon [ap1, ap2]

--------------------------------------------------------------------------------

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
             
