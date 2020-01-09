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
    , borderColor = "#D3BCA3"
    , padding = 1.5
    }


block =
    { size = 35.0
    , borderWidth = 0.5
    }


blockColors =
    { red = "#FD0000"
    , green = "#36C54C"
    , blue = "#2EA3F7"
    , orange = "#FA6600"
    , purple = "#CA55C3"
    , gray = "#989898"
    }



-- MODEL


type Location
    = Loc Int Int


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
            , stroke (.borderColor board)
            , strokeWidth (String.fromFloat borderWidth)
            ]
            []
            :: contents
        )


viewBlocks : Model -> List (Svg msg)
viewBlocks model =
    [ viewBlock (Loc 4 0) (.orange blockColors)
    , viewBlock (Loc 5 0) (.orange blockColors)
    , viewBlock (Loc 5 1) (.orange blockColors)
    , viewBlock (Loc 6 1) (.orange blockColors)
    , viewBlock (Loc 0 19) (.gray blockColors)
    , viewBlock (Loc 1 19) (.purple blockColors)
    , viewBlock (Loc 2 19) (.red blockColors)
    , viewBlock (Loc 3 19) (.orange blockColors)
    , viewBlock (Loc 7 19) (.green blockColors)
    , viewBlock (Loc 9 19) (.blue blockColors)
    ]


viewBlock : Location -> String -> Svg msg
viewBlock (Loc col row) color =
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
        , fill color
        , stroke "white"
        , strokeWidth (String.fromFloat borderWidth)
        ]
        []
