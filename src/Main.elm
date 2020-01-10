module Main exposing (..)

import Browser
import Svg exposing (Svg, rect, svg)
import Svg.Attributes exposing (..)


main : Program () Model msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- CONSTANTS


game : { columns : Int, rows : Int }
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


type Color
    = Red
    | Green
    | LightBlue
    | DarkBlue
    | Orange
    | Purple
    | Gray


type Block
    = Block Int Int Color


type Shape
    = IShape
    | JShape
    | LShape
    | OShape
    | SShape
    | TShape
    | ZShape


type alias Model =
    { fallingPiece : List Block
    , bottomBlocks : List Block
    }


init : Model
init =
    { fallingPiece =
        shapeToBlocks ZShape |> centerBlocks
    , bottomBlocks =
        [ Block 0 19 Gray
        , Block 1 19 Purple
        , Block 2 19 Red
        , Block 3 19 Orange
        , Block 7 19 Green
        , Block 8 19 DarkBlue
        , Block 9 19 LightBlue
        ]
    }



-- UPDATE


update : msg -> Model -> Model
update msg model =
    model


shapeToBlocks : Shape -> List Block
shapeToBlocks shape =
    case shape of
        IShape ->
            [ Block 0 0 Red
            , Block 1 0 Red
            , Block 2 0 Red
            , Block 3 0 Red
            ]

        JShape ->
            [ Block 0 0 LightBlue
            , Block 0 1 LightBlue
            , Block 1 1 LightBlue
            , Block 2 1 LightBlue
            ]

        LShape ->
            [ Block 2 0 Green
            , Block 0 1 Green
            , Block 1 1 Green
            , Block 2 1 Green
            ]

        OShape ->
            [ Block 0 0 Gray
            , Block 1 0 Gray
            , Block 0 1 Gray
            , Block 1 1 Gray
            ]

        SShape ->
            [ Block 1 0 DarkBlue
            , Block 2 0 DarkBlue
            , Block 0 1 DarkBlue
            , Block 1 1 DarkBlue
            ]

        TShape ->
            [ Block 1 0 Purple
            , Block 0 1 Purple
            , Block 1 1 Purple
            , Block 2 1 Purple
            ]

        ZShape ->
            [ Block 0 0 Orange
            , Block 1 0 Orange
            , Block 1 1 Orange
            , Block 2 1 Orange
            ]


centerBlocks : List Block -> List Block
centerBlocks blocks =
    let
        columns =
            List.map (\(Block col _ _) -> col) blocks

        min =
            List.minimum columns |> Maybe.withDefault 0

        max =
            List.maximum columns |> Maybe.withDefault 0

        colShift =
            -min + (game.columns - (max - min + 1)) // 2
    in
    List.map (\(Block col row color) -> Block (col + colShift) row color) blocks



-- VIEW


view : Model -> Svg msg
view model =
    viewBoard (viewBlocks model)


viewBoard : List (Svg msg) -> Svg msg
viewBoard contents =
    let
        boardSize blockCount =
            toFloat blockCount * block.size + 2 * (board.borderWidth + board.padding)

        boardWidth =
            boardSize game.columns

        boardHeight =
            boardSize game.rows
    in
    svg
        [ width "100%"
        , height (String.fromFloat (board.marginTop + boardHeight))
        , viewBox
            (String.fromFloat -(board.borderWidth + board.padding)
                ++ " "
                ++ String.fromFloat -(board.marginTop + board.borderWidth + board.padding)
                ++ " "
                ++ String.fromFloat boardWidth
                ++ " "
                ++ String.fromFloat (board.marginTop + boardHeight)
            )
        ]
        (rect
            [ x (String.fromFloat -(board.borderWidth / 2 + board.padding))
            , y (String.fromFloat -(board.borderWidth / 2 + board.padding))
            , width (String.fromFloat (boardWidth - board.borderWidth))
            , height (String.fromFloat (boardHeight - board.borderWidth))
            , fill "transparent"
            , stroke (.borderColor board)
            , strokeWidth (String.fromFloat board.borderWidth)
            ]
            []
            :: contents
        )


viewBlocks : Model -> List (Svg msg)
viewBlocks model =
    List.map viewBlock (model.fallingPiece ++ model.bottomBlocks)


viewBlock : Block -> Svg msg
viewBlock (Block col row color) =
    rect
        [ x (String.fromFloat (toFloat col * block.size + block.borderWidth / 2))
        , y (String.fromFloat (toFloat row * block.size + block.borderWidth / 2))
        , width (String.fromFloat (block.size - block.borderWidth))
        , height (String.fromFloat (block.size - block.borderWidth))
        , fill (colorToHex color)
        , stroke "white"
        , strokeWidth (String.fromFloat block.borderWidth)
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
