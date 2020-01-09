module Main exposing (main)

import Browser
import Svg exposing (Svg, rect, svg)
import Svg.Attributes exposing (..)


main : Program () () msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    ()


init : Model
init =
    ()



-- UPDATE


update : msg -> Model -> Model
update msg model =
    ()



-- VIEW


view : Model -> Svg msg
view model =
    svg
        [ width "100%"
        , height "700"
        , viewBox "0 0 350 700"
        ]
        [ rect
            [ x "0"
            , y "0"
            , width "350"
            , height "700"
            , fill "transparent"
            , stroke "black"
            , strokeWidth "1"
            ]
            []
        ]
