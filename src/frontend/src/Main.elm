module Main exposing (..)

import Browser

import Html exposing (Html, Attribute, a, br, button, div, h1, input, p
                     , text, textarea)

import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (disabled, href, placeholder, style)
import Http exposing (..) -- (Body, jsonBody)
import Json.Decode exposing (Decoder, decodeString, field, list, map, string)
import Json.Encode exposing (Value, object, string)
import Debug exposing (log)

import List exposing (append, concat)

type alias SourceCode = {src : String}
type alias ProgOutput = {out : List String}


main =
  Browser.element
      { init = init -- "" ""
      , view = view
      , update = update
      , subscriptions = subscriptions
      }


-- MODEL

type alias Model =
  { src    : String
  , output : String
  , cfg : Cfg
  }

  
type alias Cfg =
  { scheme : String
  , host : String
  , prt : String
  }

init : Cfg -> (Model, Cmd Msg)
       
init cfg =
    ( Model "" "" cfg
    , Cmd.none
    )


-- UPDATE

type Msg =
    Run
  | SaveSrc String
  | NewOutput (Result Http.Error ProgOutput)

 
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
      SaveSrc newSrc ->
        ({ model | src = newSrc}, Cmd.none)
          
      Run  ->
        (model, runProgram model)
            
      NewOutput (Err err) ->
          ( {model | output = showError err}, Cmd.none)

      NewOutput (Ok out) ->
          ( {model | output = String.concat(List.intersperse "\n" out.out)}
          , Cmd.none)
              

-- VIEW

view : Model -> Html Msg
view model =
  div centerStyle
    [
      h1 h1Style [text "Airport Groups"]
    , textarea ( srcStyle ++ [placeholder "Enter group definitions here"]
               ++ [onInput SaveSrc] )  []
    , p [] []
    , button [fontSize, onClick Run ] [ text "Run" ]
    , p [] []
    , textarea (append outStyle [disabled True]) [ text model.output]
    , p [] []
    , a [href "help.html"] [text "Help"]
    ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

      
-- HTTP

mkSourceCode : String -> SourceCode
mkSourceCode src = SourceCode src

encodeSourceCode : SourceCode -> Value
encodeSourceCode sc  = object [("src", Json.Encode.string sc.src)]

mkBody : String -> Body
mkBody src = jsonBody (encodeSourceCode (mkSourceCode src))

progOutputDecoder : Decoder ProgOutput
progOutputDecoder =
    map ProgOutput (field "out" (Json.Decode.list Json.Decode.string))


mkUrl : Cfg -> String -> String

mkUrl cfg path =
  let portStr = if cfg.prt == "" then "" else ":" ++ cfg.prt
  in cfg.scheme ++ "://" ++ cfg.host ++ portStr ++ "/" ++ path

{-
runProgramOld : Model -> Cmd Msg
runProgramOld m =
    let
      url = mkUrl m.cfg "airport-group"
        --url = "http://localhost:80/airport-group"
        -- url = "http://beta.planit9.com/airport-group"
      req = Http.post url (mkBody m.src) progOutputDecoder
    in
      Http.send NewOutput req
-}

runProgram : Model -> Cmd Msg
runProgram m =
--    let
--      req = Http.post url (mkBody m.src) progOutputDecoder
--    in
      Http.post
        { url = mkUrl m.cfg "airport-group"
        , body = mkBody m.src
        , expect = Http.expectJson NewOutput progOutputDecoder
        }

      
-- Styles

srcStyle : List (Attribute msg)
srcStyle =
    [ style "font-family" "courier"
    , fontSize
    , style "height" "300px"
    , style "width" "80%"
    ]

outStyle : List (Attribute msg)
outStyle =
    [ style "font-family" "courier"
    , style "height" "300px"
    , style "width" "80%"
    , fontSize
    ]

textStyle : List (Attribute msg)
textStyle =
    [ style "font-family" "courier"
    , fontSize
    ]

fontSize : Attribute msg
fontSize = style "font-size" "14px"

           {-
h1Style =
  style
    [ ("font-family", "sans-serif")
    ]
-}

h1Style =
  [style "font-family" "sans-serif"]

centerStyle = [style "text-align" "center"]
leftStyle = [style "text-align" "left"]

showError : Http.Error -> String
showError err =
  case err of
      BadUrl str -> "Bad URL: " ++ str
      Timeout -> "Timeout"
      NetworkError -> "NetworkError"
      BadStatus stat -> "Bad Status: " ++ String.fromInt stat
      BadBody bod -> "Bad Body: " ++ bod
