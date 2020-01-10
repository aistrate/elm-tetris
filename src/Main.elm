module Main exposing (main)

import Browser
import Svg exposing (Svg, rect, svg)
import Svg.Attributes exposing (..)


main : Program () Model msg
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


type alias Model =
    { fallingPiece : List Block
    , bottomBlocks : List Block
    }


init : Model
init =
    { fallingPiece =
        [ Block 4 0 Orange
        , Block 5 0 Orange
        , Block 5 1 Orange
        , Block 6 1 Orange
        ]
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



-- VIEW


view : Model -> Svg msg
view model =
    viewBoard (viewBlocks model)


viewBoard : List (Svg msg) -> Svg msg
viewBoard contents =
    let
        boardSize blockCount =
            blockCount * block.size + 2 * (board.borderWidth + board.padding)

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
