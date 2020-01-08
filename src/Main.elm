module Main exposing (main)

import Browser
import Html exposing (Html, text)


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


view : Model -> Html msg
view model =
    text "Hello"
