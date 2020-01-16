module Main exposing (..)

import Browser
import Browser.Events as Events
import Dict exposing (Dict)
import Json.Decode as Decode
import Process
import Random
import Svg exposing (Svg, g, rect, svg)
import Svg.Attributes exposing (..)
import Svg.Lazy exposing (lazy, lazy2)
import Task


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


boardStyle =
    { marginTop = 12.0
    , borderWidth = 2.0
    , borderColor = "#D3BCA3"
    , padding = 1.5
    }


blockStyle =
    { size = 35.0
    , borderWidth = 0.5
    }



-- MODEL


type Color
    = Red
    | Green
    | Blue
    | Cyan
    | Orange
    | Purple
    | Yellow
    | Gray


type Block
    = Block Int Int Color


type alias Tetromino =
    { blocks : List Block
    , pivot : ( Float, Float )
    }


type alias Model =
    { fallingPiece : Tetromino
    , ghostPiece : List Block
    , bottomBlocks : List Block
    , occupiedCells : Dict ( Int, Int ) ()
    , duringLockDelay : Bool
    , showGhostPiece : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { fallingPiece = emptyTetromino
      , ghostPiece = []
      , bottomBlocks = []
      , occupiedCells = Dict.fromList []
      , duringLockDelay = False
      , showGhostPiece = False
      }
    , Random.generate ShapeGenerated shapeGenerator
    )



-- UPDATE


type Shape
    = IShape
    | JShape
    | LShape
    | OShape
    | SShape
    | TShape
    | ZShape


type Msg
    = MoveLeft
    | MoveRight
    | MoveDown
    | RotateClockwise
    | RotateCounterclockwise
    | HardDrop
    | LockToBottom
    | ShapeGenerated Shape
    | ToggleGhostPiece
    | OtherKey


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveLeft ->
            ( updateFallingPiece
                model
                (detectCollision (shiftBy ( -1, 0 ) model.fallingPiece) model)
            , Cmd.none
            )

        MoveRight ->
            ( updateFallingPiece
                model
                (detectCollision (shiftBy ( 1, 0 ) model.fallingPiece) model)
            , Cmd.none
            )

        MoveDown ->
            ( updateFallingPiece
                model
                (detectCollision (shiftBy ( 0, 1 ) model.fallingPiece) model)
            , Cmd.none
            )

        RotateClockwise ->
            ( updateFallingPiece
                model
                (withinBoundsHoriz (rotateClockwise model.fallingPiece))
            , Cmd.none
            )

        RotateCounterclockwise ->
            ( updateFallingPiece
                model
                (withinBoundsHoriz (rotateCounterclockwise model.fallingPiece))
            , Cmd.none
            )

        HardDrop ->
            if not model.duringLockDelay then
                ( { model
                    | fallingPiece = dropToTarget model.fallingPiece model.ghostPiece
                    , duringLockDelay = True
                  }
                , Process.sleep 500 |> Task.perform (always LockToBottom)
                )

            else
                ( model
                , Cmd.none
                )

        LockToBottom ->
            ( lockToBottom model
            , Random.generate ShapeGenerated shapeGenerator
            )

        ShapeGenerated shape ->
            ( updateFallingPiece
                model
                (spawnTetromino shape |> centerHoriz |> shiftBy ( 0, 1 ))
            , Cmd.none
            )

        ToggleGhostPiece ->
            ( { model | showGhostPiece = not model.showGhostPiece }
            , Cmd.none
            )

        OtherKey ->
            ( model
            , Cmd.none
            )


updateFallingPiece : Model -> Tetromino -> Model
updateFallingPiece model fallingPiece =
    { model
        | fallingPiece = fallingPiece
        , ghostPiece = calculateGhostPiece fallingPiece.blocks model.occupiedCells
    }


calculateGhostPiece : List Block -> Dict ( Int, Int ) () -> List Block
calculateGhostPiece fallingPieceBlocks occupiedCells =
    let
        ghostCandidate =
            List.map (\(Block col row _) -> Block col row Gray) fallingPieceBlocks

        moveAllWayDown blocks =
            let
                nextCandidate =
                    List.map (\(Block col row color) -> Block col (row + 1) color) blocks
            in
            if collision nextCandidate occupiedCells then
                blocks

            else
                moveAllWayDown nextCandidate
    in
    moveAllWayDown ghostCandidate


dropToTarget : Tetromino -> List Block -> Tetromino
dropToTarget tetromino target =
    let
        ( tetrominoMinRow, _ ) =
            rowRange tetromino.blocks

        ( targetMinRow, _ ) =
            rowRange target
    in
    shiftBy ( 0, targetMinRow - tetrominoMinRow ) tetromino


lockToBottom : Model -> Model
lockToBottom model =
    let
        droppedPiece =
            dropToTarget model.fallingPiece model.ghostPiece

        bottomBlocks =
            removeFullRows (model.bottomBlocks ++ droppedPiece.blocks)

        occupiedCells =
            List.map (\(Block col row _) -> ( ( col, row ), () )) bottomBlocks
                |> Dict.fromList
    in
    { model
        | fallingPiece = emptyTetromino
        , ghostPiece = []
        , bottomBlocks = bottomBlocks
        , occupiedCells = occupiedCells
        , duringLockDelay = False
    }


removeFullRows : List Block -> List Block
removeFullRows blocks =
    let
        ( minRow, maxRow ) =
            rowRange blocks

        rowCounts : List ( Int, Int )
        rowCounts =
            List.range minRow maxRow
                |> List.map
                    (\row ->
                        List.filter (\(Block _ r _) -> r == row) blocks
                            |> (\bs -> ( row, List.length bs ))
                    )

        fullRows : List Int
        fullRows =
            List.filter (\( _, count ) -> count == game.columns) rowCounts
                |> List.map (\( row, _ ) -> row)
    in
    List.foldl removeRow blocks fullRows


removeRow : Int -> List Block -> List Block
removeRow row blocks =
    let
        remainingBlocks =
            List.filter (\(Block _ r _) -> r /= row) blocks

        shiftBlock (Block c r color) =
            if r < row then
                Block c (r + 1) color

            else
                Block c r color
    in
    List.map shiftBlock remainingBlocks


shapeGenerator : Random.Generator Shape
shapeGenerator =
    Random.uniform IShape [ JShape, LShape, OShape, SShape, TShape, ZShape ]


withinBoundsHoriz : Tetromino -> Tetromino
withinBoundsHoriz tetromino =
    let
        ( min, max ) =
            columnRange tetromino.blocks
    in
    if min < 0 then
        shiftBy ( -min, 0 ) tetromino

    else if max > game.columns - 1 then
        shiftBy ( game.columns - 1 - max, 0 ) tetromino

    else
        tetromino


detectCollision : Tetromino -> Model -> Tetromino
detectCollision fallingPiece model =
    if not (collision fallingPiece.blocks model.occupiedCells) then
        fallingPiece

    else
        model.fallingPiece


collision : List Block -> Dict ( Int, Int ) () -> Bool
collision blocks occupiedCells =
    let
        blockCollision (Block col row _) =
            not (0 <= col && col < game.columns && row < game.rows)
                || Dict.member ( col, row ) occupiedCells
    in
    List.any blockCollision blocks


shiftBy : ( Int, Int ) -> Tetromino -> Tetromino
shiftBy ( colDelta, rowDelta ) tetromino =
    let
        ( colPivot, rowPivot ) =
            tetromino.pivot
    in
    { blocks =
        List.map
            (\(Block col row color) -> Block (col + colDelta) (row + rowDelta) color)
            tetromino.blocks
    , pivot = ( colPivot + toFloat colDelta, rowPivot + toFloat rowDelta )
    }


flipAxes : Tetromino -> Tetromino
flipAxes tetromino =
    let
        ( colPivot, rowPivot ) =
            tetromino.pivot
    in
    { blocks = List.map (\(Block col row color) -> Block row col color) tetromino.blocks
    , pivot = ( rowPivot, colPivot )
    }


rotateClockwise : Tetromino -> Tetromino
rotateClockwise tetromino =
    flipAxes tetromino
        |> rotateCounterclockwise
        |> flipAxes


rotateCounterclockwise : Tetromino -> Tetromino
rotateCounterclockwise tetromino =
    let
        ( pivotCol, pivotRow ) =
            pivot tetromino.blocks

        rotateBlock (Block col row color) =
            Block (round (pivotCol + (toFloat row - pivotRow)))
                (round (pivotRow - (toFloat col - pivotCol)))
                color
    in
    { blocks = List.map rotateBlock tetromino.blocks
    , pivot = tetromino.pivot
    }


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


centerHoriz : Tetromino -> Tetromino
centerHoriz tetromino =
    let
        ( min, max ) =
            columnRange tetromino.blocks

        delta =
            -min + (game.columns - (max - min + 1)) // 2
    in
    shiftBy ( delta, 0 ) tetromino


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


spawnTetromino : Shape -> Tetromino
spawnTetromino shape =
    case shape of
        IShape ->
            { blocks =
                [ Block 0 1 Cyan
                , Block 1 1 Cyan
                , Block 2 1 Cyan
                , Block 3 1 Cyan
                ]
            , pivot = ( 1.5, 1.5 )
            }

        JShape ->
            { blocks =
                [ Block 0 0 Blue
                , Block 0 1 Blue
                , Block 1 1 Blue
                , Block 2 1 Blue
                ]
            , pivot = ( 1, 1 )
            }

        LShape ->
            { blocks =
                [ Block 2 0 Orange
                , Block 0 1 Orange
                , Block 1 1 Orange
                , Block 2 1 Orange
                ]
            , pivot = ( 1, 1 )
            }

        OShape ->
            { blocks =
                [ Block 0 0 Yellow
                , Block 1 0 Yellow
                , Block 0 1 Yellow
                , Block 1 1 Yellow
                ]
            , pivot = ( 0.5, 0.5 )
            }

        SShape ->
            { blocks =
                [ Block 1 0 Green
                , Block 2 0 Green
                , Block 0 1 Green
                , Block 1 1 Green
                ]
            , pivot = ( 1, 1 )
            }

        TShape ->
            { blocks =
                [ Block 1 0 Purple
                , Block 0 1 Purple
                , Block 1 1 Purple
                , Block 2 1 Purple
                ]
            , pivot = ( 1, 1 )
            }

        ZShape ->
            { blocks =
                [ Block 0 0 Red
                , Block 1 0 Red
                , Block 1 1 Red
                , Block 2 1 Red
                ]
            , pivot = ( 1, 1 )
            }


emptyTetromino : Tetromino
emptyTetromino =
    { blocks = []
    , pivot = ( 1, 1 )
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Events.onKeyDown keyDecoder


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

        "arrowdown" ->
            MoveDown

        "arrowup" ->
            RotateClockwise

        "x" ->
            RotateClockwise

        "control" ->
            RotateCounterclockwise

        "z" ->
            RotateCounterclockwise

        " " ->
            HardDrop

        "g" ->
            ToggleGhostPiece

        _ ->
            OtherKey



-- VIEW


boardWidth : Float
boardWidth =
    boardSize game.columns


boardHeight : Float
boardHeight =
    boardSize game.rows


boardSize : Int -> Float
boardSize blockCount =
    toFloat blockCount * blockStyle.size + 2 * (boardStyle.borderWidth + boardStyle.padding)


view : Model -> Svg Msg
view model =
    lazy viewGame model


viewGame : Model -> Svg Msg
viewGame model =
    svg
        [ width "100%"
        , height (String.fromFloat (boardStyle.marginTop + boardHeight))
        , viewBox
            (String.fromFloat -(boardStyle.borderWidth + boardStyle.padding)
                ++ " "
                ++ String.fromFloat -(boardStyle.marginTop + boardStyle.borderWidth + boardStyle.padding)
                ++ " "
                ++ String.fromFloat boardWidth
                ++ " "
                ++ String.fromFloat (boardStyle.marginTop + boardHeight)
            )
        ]
        [ lazy viewBoard ()
        , lazy2 viewGhostPiece model.showGhostPiece model.ghostPiece
        , lazy viewBlocks model.fallingPiece.blocks
        , lazy viewBlocks model.bottomBlocks
        ]


viewBoard : () -> Svg Msg
viewBoard _ =
    rect
        [ x (String.fromFloat -(boardStyle.borderWidth / 2 + boardStyle.padding))
        , y (String.fromFloat -(boardStyle.borderWidth / 2 + boardStyle.padding))
        , width (String.fromFloat (boardWidth - boardStyle.borderWidth))
        , height (String.fromFloat (boardHeight - boardStyle.borderWidth))
        , fill "transparent"
        , stroke boardStyle.borderColor
        , strokeWidth (String.fromFloat boardStyle.borderWidth)
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
            [ x (String.fromFloat (toFloat col * blockStyle.size + blockStyle.borderWidth / 2))
            , y (String.fromFloat (toFloat row * blockStyle.size + blockStyle.borderWidth / 2))
            , width (String.fromFloat (blockStyle.size - blockStyle.borderWidth))
            , height (String.fromFloat (blockStyle.size - blockStyle.borderWidth))
            , fill (colorToHex color)
            , stroke "white"
            , strokeWidth (String.fromFloat blockStyle.borderWidth)
            ]
            []

    else
        rect [] []


viewGhostPiece : Bool -> List Block -> Svg Msg
viewGhostPiece showGhostPiece blocks =
    if showGhostPiece then
        viewBlocks blocks

    else
        g [] []


colorToHex : Color -> String
colorToHex color =
    case color of
        Red ->
            "#FD0000"

        Green ->
            "#36C54C"

        Blue ->
            "#3968B0"

        Cyan ->
            "#2EA3F7"

        Orange ->
            "#FA6600"

        Purple ->
            "#CA55C3"

        Yellow ->
            "#F2D00D"

        Gray ->
            "#DDD"
