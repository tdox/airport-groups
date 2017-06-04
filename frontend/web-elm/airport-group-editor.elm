import Html exposing (Html, Attribute, button, div, h1, input, p, text, textarea)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
-- import Http


main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type alias Model =
  { src    : String
  , output : String
  }

model : Model
model =
  { src = ""
  , output = ""
  }


-- UPDATE

type Msg =
    Run
  | SaveSrc String
--  | NewOutput (Result Http.Error String)

update : Msg -> Model -> Model
update msg model =
  case msg of
      SaveSrc newSrc ->
        { model | src = newSrc}
          
      Run  ->
        { model | output = model.src}


-- VIEW

view : Model -> Html Msg
view model =
  div [ centerStyle ]
    [ -- button [ onClick Decrement ] [ text "-" ]
--      div [] [ text model.src ]
--      input [ placeholder "Enter group definitions here", onInput Interpret ]  []
      h1 [h1Style] [text "Airport Groups"]
    , textarea [ srcStyle, placeholder "Enter group definitions here", onInput SaveSrc ]  []
    , p [] []
    , button [ style [fontSize], onClick Run ] [ text "Run" ]
    , p [] []
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

