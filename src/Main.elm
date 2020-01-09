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
    , marginTop = 12.0
    , borderWidth = 2.0
    , padding = 1.5
    }


block =
    { size = 35.0
    , borderWidth = 0.5
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
            blocks * .size block + 2 * (.borderWidth board + .padding board)

        boardWidth =
            boardSize (.columns board)

        boardHeight =
            boardSize (.rows board)
    in
    svg
        [ width "100%"
        , height (String.fromFloat (.marginTop board + boardHeight))
        , viewBox
            (String.fromFloat -(.borderWidth board + .padding board)
                ++ " "
                ++ String.fromFloat -(.marginTop board + .borderWidth board + .padding board)
                ++ " "
                ++ String.fromFloat boardWidth
                ++ " "
                ++ String.fromFloat (.marginTop board + boardHeight)
            )
        ]
        [ rect
            [ x (String.fromFloat -(.borderWidth board / 2 + .padding board))
            , y (String.fromFloat -(.borderWidth board / 2 + .padding board))
            , width (String.fromFloat (boardWidth - .borderWidth board))
            , height (String.fromFloat (boardHeight - .borderWidth board))
            , fill "transparent"
            , stroke "#D3BCA3"
            , strokeWidth (String.fromFloat (.borderWidth board))
            ]
            []
        , viewBlock ( 4, 0 )
        , viewBlock ( 5, 0 )
        , viewBlock ( 5, 1 )
        , viewBlock ( 6, 1 )
        , viewBlock ( 0, 19 )
        , viewBlock ( 1, 19 )
        , viewBlock ( 2, 19 )
        , viewBlock ( 7, 19 )
        , viewBlock ( 9, 19 )
        ]


viewBlock : ( Int, Int ) -> Svg msg
viewBlock ( col, row ) =
    let
        size =
            .size block

        borderWidth =
            .borderWidth block
    in
    rect
        [ x (String.fromFloat (toFloat col * size + borderWidth / 2))
        , y (String.fromFloat (toFloat row * size + borderWidth / 2))
        , width (String.fromFloat (size - borderWidth))
        , height (String.fromFloat (size - borderWidth))
        , fill "silver"
        , stroke "white"
        , strokeWidth (String.fromFloat borderWidth)
        ]
        []
