{-# LANGUAGE OverloadedStrings #-}

module AirportGroups where

-- text
import Data.Text (Text)
import qualified Data.Text as T
-- import Groups

{-







-}


type CountryCode = Text
type UsStateCode = (Char, Char)
--type AirportCode = Text


--data AirportCodeType = ICAOtype | IATAtype | CACtype

data ICAO = ICAO Text deriving (Eq, Ord, Show)
data IATA = IATA Text deriving (Eq, Ord, Show)
data FAA  = FAA  Text deriving (Eq, Ord, Show)

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

data ID a = ID {iD :: Int} deriving (Eq, Show)

type LatLonDeg = (Double, Double)



--data Airport = Airport AirportCodes deriving (Eq, Show)
data Airport = Airport { apId          :: ID Airport
                       , apCodes       :: AirportCodes
                       , apCountryCode :: CountryCode
                       , apUsStateCode :: Maybe (UsStateCode)
                       , apLatLon      :: LatLonDeg
                       } deriving (Eq, Show)
  

{-

<program> ::=  <stmt> ";"  | <stmt> ";" <program>

<stmt> ::= <set-assign-stmt>
         | <pred-assign-stmt>
         | <print-stmt>

<set-assign-stmt> ::= <set-var> "=" <set-expr>
<pred-assign-stmt> ::= <pred-var> "=" <pred-expr>
<print-stmt> ::= "print <set-expr>

<set-var> ::= <alphanumeric>

<set-expr> ::= <set-var>
             | "(" <set-expr> ")"
             | airport-identifier-list
             | <set-expr> <set-op> <set-expr>
             | <set-expr> "|" <pred-expr>

<set-op> ::= "\/" | "/\"

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
              | <pred-expr> <pred-op> <pred-expr>
              | "isInState(" <state-code> ")"
              | "isInCountry(" <country-code> ")"
              | "isNearInMiles(" <airport-identifier> "," <float> ")"
              | "isNortOfLatitudeDegs(" <float> ")"
              | "isSouthOfLatitudeDegs(" <float> ")"
              | "isSouthOfLatitudeDegs(" <float> ")"
              | "isBetweenLongitudesDegs(" <float> "," <float> ")"

pred-op ::= "&&" | "||"

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
             (33.7749, (-122.4194))
  
a2 = Airport (ID 2)
             (AirportCodes (Just (FAA "LAX")) Nothing Nothing  (Just (CAC "USA" "LAX")))
             "USA"
             (Just ('C', 'A'))
             (33.9416, (-118.4085))
             
