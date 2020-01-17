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


type ShapeSize
    = Size2By2
    | Size3By2
    | Size4By1


type
    -- see https://tetris.fandom.com/wiki/SRS
    RotationState
    = RotationState0 -- spawn state (horizontal, flat side down)
    | RotationState1 -- 1 clockwise rotation from spawn state
    | RotationState2 -- 2 successive rotations in either direction from spawn state
    | RotationState3 -- 1 counterclockwise rotation from spawn state


type alias Tetromino =
    { blocks : List Block
    , pivot : ( Float, Float )
    , shapeSize : ShapeSize
    , rotationState : RotationState
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


type RotationDirection
    = Clockwise
    | Counterclockwise


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
            ( applyTransform (shiftBy ( -1, 0 )) noAlternatives model
            , Cmd.none
            )

        MoveRight ->
            ( applyTransform (shiftBy ( 1, 0 )) noAlternatives model
            , Cmd.none
            )

        MoveDown ->
            ( applyTransform (shiftBy ( 0, 1 )) noAlternatives model
            , Cmd.none
            )

        RotateClockwise ->
            ( applyTransform (rotate Clockwise) wallKickAlternatives model
            , Cmd.none
            )

        RotateCounterclockwise ->
            ( applyTransform (rotate Counterclockwise) wallKickAlternatives model
            , Cmd.none
            )

        HardDrop ->
            if not model.duringLockDelay then
                ( { model
                    | fallingPiece = shiftVertToTarget model.fallingPiece model.ghostPiece
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
            ( applyTransform (always (spawnTetromino shape) >> centerHoriz) noAlternatives model
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


applyTransform : (Tetromino -> Tetromino) -> (Tetromino -> List ( Int, Int )) -> Model -> Model
applyTransform transform alternativeTranslations model =
    let
        viableFallingPiece =
            firstViableAlternative
                (alternativeTranslations model.fallingPiece)
                (transform model.fallingPiece)
                model.occupiedCells
                |> Maybe.withDefault model.fallingPiece
    in
    { model
        | fallingPiece = viableFallingPiece
        , ghostPiece = calculateGhostPiece viableFallingPiece.blocks model.occupiedCells
    }


firstViableAlternative : List ( Int, Int ) -> Tetromino -> Dict ( Int, Int ) () -> Maybe Tetromino
firstViableAlternative translations tetromino occupiedCells =
    case translations of
        firstTranslation :: remainingTranslations ->
            let
                alternative =
                    shiftBy firstTranslation tetromino
            in
            if not (collision alternative.blocks occupiedCells) then
                Just alternative

            else
                firstViableAlternative remainingTranslations tetromino occupiedCells

        [] ->
            Nothing


noAlternatives : Tetromino -> List ( Int, Int )
noAlternatives _ =
    [ ( 0, 0 ) ]


wallKickAlternatives : Tetromino -> List ( Int, Int )
wallKickAlternatives _ =
    [ ( 0, 0 ), ( -1, 0 ), ( 1, 0 ), ( 0, -1 ), ( -2, 0 ), ( 2, 0 ), ( 0, -2 ) ]


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
    if List.length ghostCandidate > 0 then
        moveAllWayDown ghostCandidate

    else
        []


shiftVertToTarget : Tetromino -> List Block -> Tetromino
shiftVertToTarget tetromino target =
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
            shiftVertToTarget model.fallingPiece model.ghostPiece

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
    { tetromino
        | blocks =
            List.map
                (\(Block col row color) -> Block (col + colDelta) (row + rowDelta) color)
                tetromino.blocks
        , pivot =
            ( colPivot + toFloat colDelta, rowPivot + toFloat rowDelta )
    }


rotate : RotationDirection -> Tetromino -> Tetromino
rotate direction tetromino =
    let
        ( pivotCol, pivotRow ) =
            tetromino.pivot

        sign =
            case direction of
                Clockwise ->
                    1

                Counterclockwise ->
                    -1

        rotateBlock (Block col row color) =
            Block
                (round (pivotCol - sign * (toFloat row - pivotRow)))
                (round (pivotRow + sign * (toFloat col - pivotCol)))
                color
    in
    { tetromino
        | blocks = List.map rotateBlock tetromino.blocks
        , rotationState = nextRotationState tetromino.rotationState direction
    }


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
            , shapeSize = Size4By1
            , rotationState = RotationState0
            }

        JShape ->
            { blocks =
                [ Block 0 0 Blue
                , Block 0 1 Blue
                , Block 1 1 Blue
                , Block 2 1 Blue
                ]
            , pivot = ( 1, 1 )
            , shapeSize = Size3By2
            , rotationState = RotationState0
            }

        LShape ->
            { blocks =
                [ Block 2 0 Orange
                , Block 0 1 Orange
                , Block 1 1 Orange
                , Block 2 1 Orange
                ]
            , pivot = ( 1, 1 )
            , shapeSize = Size3By2
            , rotationState = RotationState0
            }

        OShape ->
            { blocks =
                [ Block 0 0 Yellow
                , Block 1 0 Yellow
                , Block 0 1 Yellow
                , Block 1 1 Yellow
                ]
            , pivot = ( 0.5, 0.5 )
            , shapeSize = Size2By2
            , rotationState = RotationState0
            }

        SShape ->
            { blocks =
                [ Block 1 0 Green
                , Block 2 0 Green
                , Block 0 1 Green
                , Block 1 1 Green
                ]
            , pivot = ( 1, 1 )
            , shapeSize = Size3By2
            , rotationState = RotationState0
            }

        TShape ->
            { blocks =
                [ Block 1 0 Purple
                , Block 0 1 Purple
                , Block 1 1 Purple
                , Block 2 1 Purple
                ]
            , pivot = ( 1, 1 )
            , shapeSize = Size3By2
            , rotationState = RotationState0
            }

        ZShape ->
            { blocks =
                [ Block 0 0 Red
                , Block 1 0 Red
                , Block 1 1 Red
                , Block 2 1 Red
                ]
            , pivot = ( 1, 1 )
            , shapeSize = Size3By2
            , rotationState = RotationState0
            }


emptyTetromino : Tetromino
emptyTetromino =
    { blocks = []
    , pivot = ( 0.5, 0.5 )
    , shapeSize = Size2By2
    , rotationState = RotationState0
    }


nextRotationState : RotationState -> RotationDirection -> RotationState
nextRotationState currentRotationState direction =
    case ( currentRotationState, direction ) of
        ( RotationState0, Clockwise ) ->
            RotationState1

        ( RotationState1, Clockwise ) ->
            RotationState2

        ( RotationState2, Clockwise ) ->
            RotationState3

        ( RotationState3, Clockwise ) ->
            RotationState0

        ( RotationState3, Counterclockwise ) ->
            RotationState2

        ( RotationState2, Counterclockwise ) ->
            RotationState1

        ( RotationState1, Counterclockwise ) ->
            RotationState0

        ( RotationState0, Counterclockwise ) ->
            RotationState3



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
