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



-- MODEL


type Location
    = Loc Int Int


type Color
    = Red
    | Green
    | LightBlue
    | DarkBlue
    | Orange
    | Purple
    | Gray


type Block
    = Block Location Color


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
    List.map viewBlock
        [ Block (Loc 4 0) Orange
        , Block (Loc 5 0) Orange
        , Block (Loc 5 1) Orange
        , Block (Loc 6 1) Orange
        , Block (Loc 0 19) Gray
        , Block (Loc 1 19) Purple
        , Block (Loc 2 19) Red
        , Block (Loc 3 19) Orange
        , Block (Loc 7 19) Green
        , Block (Loc 8 19) DarkBlue
        , Block (Loc 9 19) LightBlue
        ]


viewBlock : Block -> Svg msg
viewBlock (Block (Loc col row) color) =
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
        , fill (colorToHex color)
        , stroke "white"
        , strokeWidth (String.fromFloat borderWidth)
        ]
        []


colorToHex : Color -> String
colorToHex color =
    case color of
        Red ->
            "#FD0000"

        Green ->
            "#36C54C"

        LightBlue ->
            "#2EA3F7"

        DarkBlue ->
            "#3968B0"

        Orange ->
            "#FA6600"

        Purple ->
            "#CA55C3"

        Gray ->
            "#989898"
