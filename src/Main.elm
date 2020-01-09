module Main exposing (main)

import Browser
import Svg exposing (Svg, rect, svg)
import Svg.Attributes exposing (..)


main : Program () () msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- CONSTANTS


board =
    { columns = 10
    , rows = 20
    , blockSize = 35.0
    , borderWidth = 2.0
    }



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
    let
        boardSize blocks =
            blocks * .blockSize board + 2 * .borderWidth board

        boardWidth =
            boardSize (.columns board)

        boardHeight =
            boardSize (.rows board)
    in
    svg
        [ width "100%"
        , height (String.fromFloat boardHeight)
        , viewBox
            (String.fromFloat -(.borderWidth board)
                ++ " "
                ++ String.fromFloat -(.borderWidth board)
                ++ " "
                ++ String.fromFloat boardWidth
                ++ " "
                ++ String.fromFloat boardHeight
            )
        ]
        [ rect
            [ x (String.fromFloat -(.borderWidth board / 2))
            , y (String.fromFloat -(.borderWidth board / 2))
            , width (String.fromFloat (boardWidth - .borderWidth board))
            , height (String.fromFloat (boardHeight - .borderWidth board))
            , fill "transparent"
            , stroke "black"
            , strokeWidth (String.fromFloat (.borderWidth board))
            ]
            []
        ]
