import Html exposing (Html, Attribute, br, button, div, h1, input, p
                     , text, textarea)

import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (disabled, placeholder, style)
import Http exposing (Body, jsonBody)
import Json.Decode exposing (Decoder, decodeString, field, list, map, string)
import Json.Encode exposing (Value, object, string)
import Debug exposing (log)

type alias SourceCode = {src : String}
type alias ProgOutput = {out : List String}


main =
  Html.program
      { init = init "" ""
      , view = view
      , update = update
      , subscriptions = subscriptions
      }


-- MODEL

type alias Model =
  { src    : String
  , output : String
  }

    
init : String -> String -> (Model, Cmd Msg)
       
init initSrc initOutput =
    ( Model initSrc initOutput
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
        (model, runProgram model.src)
            
      NewOutput (Err err) ->
          ( {model | output = toString err}, Cmd.none)

      NewOutput (Ok out) ->
          ( {model | output = String.concat(List.intersperse "\n" out.out)}
          , Cmd.none)
              

-- VIEW

view : Model -> Html Msg
view model =
  div [ centerStyle ]
    [
      h1 [h1Style] [text "Airport Groups"]
    , textarea [ srcStyle, placeholder "Enter group definitions here"
               , onInput SaveSrc ]  []
    , p [] []
    , button [ style [fontSize], onClick Run ] [ text "Run" ]
    , p [] []
    , textarea [outStyle, disabled True] [ text model.output]
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

        
runProgram : String -> Cmd Msg
runProgram src =
    let
        url = "http://localhost:80/airport-group"
        -- url = "http://beta.planit9.com/airport-group"
        req = Http.post url (mkBody src) progOutputDecoder
    in
        Http.send NewOutput req


      
-- Styles

srcStyle : Attribute msg
srcStyle =
  style
    [ ("font-family", "courier")
    , fontSize
    , ("height", "300px")
    , ("width", "80%")
    ]

outStyle : Attribute msg
outStyle =
  style
    [ ("font-family", "courier")
    , ("height", "300px")
    , ("width", "80%")
    , fontSize
    ]

textStyle : Attribute msg
textStyle =
  style
    [ ("font-family", "courier")
    , fontSize
    ]

fontSize : (String, String)
fontSize = ("font-size", "14px")

h1Style =
  style
    [ ("font-family", "sans-serif")
    ]

centerStyle = style [("text-align", "center")]
leftStyle = style [("text-align", "left")]

