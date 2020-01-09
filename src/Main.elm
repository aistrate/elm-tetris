module Main exposing (main)

import Browser
import Svg exposing (Svg, rect, svg)
import Svg.Attributes exposing (..)


main : Program () () msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- CONSTANTS


game =
    { columns = 10
    , rows = 20
    }


board =
    { marginTop = 12.0
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
    viewBoard (viewBlocks model)


viewBoard : List (Svg msg) -> Svg msg
viewBoard contents =
    let
        marginTop =
            .marginTop board

        borderWidth =
            .borderWidth board

        padding =
            .padding board

        boardSize blocks =
            blocks * .size block + 2 * (borderWidth + padding)

        boardWidth =
            boardSize (.columns game)

        boardHeight =
            boardSize (.rows game)
    in
    svg
        [ width "100%"
        , height (String.fromFloat (marginTop + boardHeight))
        , viewBox
            (String.fromFloat -(borderWidth + padding)
                ++ " "
                ++ String.fromFloat -(marginTop + borderWidth + padding)
                ++ " "
                ++ String.fromFloat boardWidth
                ++ " "
                ++ String.fromFloat (marginTop + boardHeight)
            )
        ]
        (rect
            [ x (String.fromFloat -(borderWidth / 2 + padding))
            , y (String.fromFloat -(borderWidth / 2 + padding))
            , width (String.fromFloat (boardWidth - borderWidth))
            , height (String.fromFloat (boardHeight - borderWidth))
            , fill "transparent"
            , stroke "#D3BCA3"
            , strokeWidth (String.fromFloat borderWidth)
            ]
            []
            :: contents
        )


viewBlocks : Model -> List (Svg msg)
viewBlocks model =
    [ viewBlock ( 4, 0 )
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
