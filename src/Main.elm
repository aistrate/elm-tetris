module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown)
import Json.Decode as Decode
import Random
import Svg exposing (Svg, g, rect, svg)
import Svg.Attributes exposing (..)
import Svg.Lazy exposing (lazy)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



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


boardSize blockCount =
    toFloat blockCount * block.size + 2 * (board.borderWidth + board.padding)


boardWidth =
    boardSize game.columns


boardHeight =
    boardSize game.rows



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


init : () -> ( Model, Cmd Msg )
init _ =
    ( { fallingPiece = []
      , bottomBlocks = []
      }
    , Random.generate NewShape shapeGenerator
    )



-- UPDATE


type Msg
    = MoveLeft
    | MoveRight
    | RotateCounterclockwise
    | RotateClockwise
    | DropPiece
    | NewShape Shape
    | OtherKey


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveLeft ->
            ( { model
                | fallingPiece =
                    withinBoundsHoriz (shiftHoriz -1 model.fallingPiece)
              }
            , Cmd.none
            )

        MoveRight ->
            ( { model
                | fallingPiece =
                    withinBoundsHoriz (shiftHoriz 1 model.fallingPiece)
              }
            , Cmd.none
            )

        RotateCounterclockwise ->
            ( { model
                | fallingPiece =
                    withinBoundsHoriz (rotateCounterclockwise model.fallingPiece)
              }
            , Cmd.none
            )

        RotateClockwise ->
            ( { model
                | fallingPiece =
                    withinBoundsHoriz (rotateClockwise model.fallingPiece)
              }
            , Cmd.none
            )

        DropPiece ->
            ( { model
                | fallingPiece = []
              }
            , Random.generate NewShape shapeGenerator
            )

        NewShape shape ->
            ( { model
                | fallingPiece =
                    shapeToBlocks shape |> centerHoriz |> shiftVert 2
              }
            , Cmd.none
            )

        OtherKey ->
            ( model, Cmd.none )


shapeGenerator : Random.Generator Shape
shapeGenerator =
    Random.uniform IShape [ JShape, LShape, OShape, SShape, TShape, ZShape ]


withinBoundsHoriz : List Block -> List Block
withinBoundsHoriz blocks =
    let
        ( min, max ) =
            columnRange blocks
    in
    if min < 0 then
        shiftHoriz -min blocks

    else if max > game.columns - 1 then
        shiftHoriz (game.columns - 1 - max) blocks

    else
        blocks


shiftHoriz : Int -> List Block -> List Block
shiftHoriz delta blocks =
    List.map (\(Block col row color) -> Block (col + delta) row color) blocks


shiftVert : Int -> List Block -> List Block
shiftVert delta blocks =
    List.map (\(Block col row color) -> Block col (row + delta) color) blocks


flipAxes : List Block -> List Block
flipAxes blocks =
    List.map (\(Block col row color) -> Block row col color) blocks


rotateClockwise : List Block -> List Block
rotateClockwise blocks =
    flipAxes blocks
        |> rotateCounterclockwise
        |> flipAxes


rotateCounterclockwise : List Block -> List Block
rotateCounterclockwise blocks =
    let
        ( pivotCol, pivotRow ) =
            pivot blocks

        rotateBlock (Block col row color) =
            Block (round (pivotCol + (toFloat row - pivotRow)))
                (round (pivotRow - (toFloat col - pivotCol)))
                color
    in
    List.map rotateBlock blocks


pivot : List Block -> ( Float, Float )
pivot blocks =
    let
        ( minCol, maxCol ) =
            columnRange blocks

        ( minRow, maxRow ) =
            rowRange blocks

        width =
            maxCol - minCol + 1

        height =
            maxRow - minRow + 1

        ( relCol, relRow ) =
            relativePivot ( width, height )
    in
    ( toFloat minCol + relCol, toFloat minRow + relRow )


relativePivot : ( Int, Int ) -> ( Float, Float )
relativePivot size =
    case size of
        ( 4, 1 ) ->
            ( 1.5, -0.5 )

        ( 1, 4 ) ->
            ( 0, 2 )

        ( 3, 2 ) ->
            ( 1, 0 )

        ( 2, 3 ) ->
            ( 0.5, 1.5 )

        ( 2, 2 ) ->
            ( 0.5, 0.5 )

        _ ->
            ( 0, 0 )


centerHoriz : List Block -> List Block
centerHoriz blocks =
    let
        ( min, max ) =
            columnRange blocks

        delta =
            -min + (game.columns - (max - min + 1)) // 2
    in
    shiftHoriz delta blocks


columnRange : List Block -> ( Int, Int )
columnRange blocks =
    range (\(Block col _ _) -> col) blocks


rowRange : List Block -> ( Int, Int )
rowRange blocks =
    range (\(Block _ row _) -> row) blocks


range : (Block -> Int) -> List Block -> ( Int, Int )
range value blocks =
    let
        values =
            List.map value blocks

        min =
            List.minimum values |> Maybe.withDefault 0

        max =
            List.maximum values |> Maybe.withDefault -1
    in
    ( min, max )


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyDown keyDecoder


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKeyboardMsg (Decode.field "key" Decode.string)


toKeyboardMsg : String -> Msg
toKeyboardMsg key =
    case String.toLower key of
        "arrowleft" ->
            MoveLeft

        "arrowright" ->
            MoveRight

        "arrowup" ->
            RotateCounterclockwise

        "arrowdown" ->
            RotateClockwise

        " " ->
            DropPiece

        _ ->
            OtherKey



-- VIEW


view : Model -> Svg Msg
view model =
    lazy viewGame model


viewGame : Model -> Svg Msg
viewGame model =
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
        [ lazy viewBoard ()
        , lazy viewBlocks model.fallingPiece
        , lazy viewBlocks model.bottomBlocks
        ]


viewBoard : () -> Svg Msg
viewBoard _ =
    rect
        [ x (String.fromFloat -(board.borderWidth / 2 + board.padding))
        , y (String.fromFloat -(board.borderWidth / 2 + board.padding))
        , width (String.fromFloat (boardWidth - board.borderWidth))
        , height (String.fromFloat (boardHeight - board.borderWidth))
        , fill "transparent"
        , stroke (.borderColor board)
        , strokeWidth (String.fromFloat board.borderWidth)
        ]
        []


viewBlocks : List Block -> Svg Msg
viewBlocks blocks =
    g
        []
        (List.map
            viewBlock
            blocks
        )


viewBlock : Block -> Svg Msg
viewBlock (Block col row color) =
    if 0 <= col && col < game.columns && 0 <= row && row < game.rows then
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

    else
        rect [] []


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
