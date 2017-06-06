import Html exposing (Html, Attribute, br, button, div, h1, input, p, text, textarea)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (disabled, placeholder, style)
import Http exposing (Body, jsonBody)
import Json.Decode exposing (Decoder, decodeString, field, list, map, string)
import Json.Encode exposing (Value)
import Debug exposing (log)

-- import Http

type alias SourceCode = {src : String}
type alias ProgOutput = {out : List String}


main =
  Html.program
      { init = init "" ""
      , view = view
      , update = update
      , subscriptions = subscriptions
      }
--  Html.beginnerProgram { model = model, view = view, update = update }


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


    {-
model : Model
model =
  { src = ""
  , output = ""
  }
-}


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
          
--      Run  ->
--        { model | output = model.src}

      Run  ->
        (model, runProgram model.src)
            
      NewOutput (Err err) ->
--          ( model, Cmd.none)
          ( {model | output = toString err}, Cmd.none)

      NewOutput (Ok out) ->
          ( {model | output = String.concat out.out}, Cmd.none)
              
          


-- VIEW

view : Model -> Html Msg
view model =
  div [ centerStyle ]
    [ -- button [ onClick Decrement ] [ text "-" ]
--      div [] [ text model.src ]
--      input [ placeholder "Enter group definitions here", onInput Interpret ]  []
      h1 [h1Style] [text "Airport Groups"]
    , textarea [ srcStyle, placeholder "Enter group definitions here", onInput SaveSrc ]  []
    , br [] []
    , button [ style [fontSize], onClick Run ] [ text "Run" ]
    , br [] []
    , textarea [outStyle, disabled True] [ text model.output]
    -- , div [textStyle] [ text model.output]
    --, button [ onClick Increment ] [ text "+" ]
    ]

-- textarea : List (Attribute msg) -> List (Html msg) -> Html msg
-- input : List (Attribute msg) -> List (Html msg) -> Html msg
-- button :  List (Attribute msg) -> List (Html msg) -> Html msg
-- type alias Attribute msg = Property msg
-- text : String -> Html msg
-- onInput : (String -> msg) -> Attribute msg
-- placeholder : String -> Attribute msg
-- style : List (String, String) -> Attribute msg

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- HTTP


mkSourceCode : String -> SourceCode
mkSourceCode src = SourceCode src

encodeSourceCode : SourceCode -> Value
encodeSourceCode sc  = Json.Encode.string sc.src

mkBody : String -> Body
mkBody src = jsonBody (encodeSourceCode (mkSourceCode src))

progOutputDecoder : Decoder ProgOutput
progOutputDecoder = map ProgOutput (field "out" (list string))
--progOutputDecoder = decodeString (field "out" (list string))
--progOutputDecoder = ProgOutput (field "out" (list string))


runProgram : String -> Cmd Msg
runProgram src =
    let
        url = "http://localhost:8080/airport-group"
        req = Http.post url (mkBody src) progOutputDecoder
        reqStr = log "req" req
    in
        Http.send NewOutput req
--        Http.send NewOutput (Http.post url (mkBody src) progOutputDecoder)

-- send : (Result Error a -> msg) -> Request a -> Cmd msg
-- post : 
      
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
--    , ("contenteditable", "false")
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
--    , ("text-align", "left")
    ]

centerStyle = style [("text-align", "center")]
leftStyle = style [("text-align", "left")]

