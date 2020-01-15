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


type alias Model =
    { fallingPiece : List Block
    , ghostPiece : List Block
    , bottomBlocks : List Block
    , occupiedCells : Dict ( Int, Int ) ()
    , duringLockDelay : Bool
    , showGhostPiece : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { fallingPiece = []
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
                (detectCollision (shiftHoriz -1 model.fallingPiece) model)
            , Cmd.none
            )

        MoveRight ->
            ( updateFallingPiece
                model
                (detectCollision (shiftHoriz 1 model.fallingPiece) model)
            , Cmd.none
            )

        MoveDown ->
            ( updateFallingPiece
                model
                (detectCollision (shiftVert 1 model.fallingPiece) model)
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
                    | fallingPiece = dropToBottom model.fallingPiece model.ghostPiece
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
                (shapeToBlocks shape |> centerHoriz |> shiftVert 1)
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


updateFallingPiece : Model -> List Block -> Model
updateFallingPiece model fallingPiece =
    { model
        | fallingPiece = fallingPiece
        , ghostPiece = calculateGhostPiece fallingPiece model.occupiedCells
    }


calculateGhostPiece : List Block -> Dict ( Int, Int ) () -> List Block
calculateGhostPiece fallingPiece occupiedCells =
    let
        ghostCandidate =
            List.map (\(Block col row _) -> Block col row Gray) fallingPiece

        moveAllWayDown blocks =
            let
                nextCandidate =
                    shiftVert 1 blocks
            in
            if collision nextCandidate occupiedCells then
                blocks

            else
                moveAllWayDown nextCandidate
    in
    moveAllWayDown ghostCandidate


dropToBottom : List Block -> List Block -> List Block
dropToBottom fallingPiece ghostPiece =
    let
        ( fallingPieceMinRow, _ ) =
            rowRange fallingPiece

        ( ghostPieceMinRow, _ ) =
            rowRange ghostPiece
    in
    shiftVert (ghostPieceMinRow - fallingPieceMinRow) fallingPiece


lockToBottom : Model -> Model
lockToBottom model =
    let
        droppedPiece =
            dropToBottom model.fallingPiece model.ghostPiece

        bottomBlocks =
            removeFullRows (model.bottomBlocks ++ droppedPiece)

        occupiedCells =
            List.map (\(Block col row _) -> ( ( col, row ), () )) bottomBlocks
                |> Dict.fromList
    in
    { model
        | fallingPiece = []
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


detectCollision : List Block -> Model -> List Block
detectCollision fallingPiece model =
    if not (collision fallingPiece model.occupiedCells) then
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
            [ Block 0 1 Cyan
            , Block 1 1 Cyan
            , Block 2 1 Cyan
            , Block 3 1 Cyan
            ]

        JShape ->
            [ Block 0 0 Blue
            , Block 0 1 Blue
            , Block 1 1 Blue
            , Block 2 1 Blue
            ]

        LShape ->
            [ Block 2 0 Orange
            , Block 0 1 Orange
            , Block 1 1 Orange
            , Block 2 1 Orange
            ]

        OShape ->
            [ Block 0 0 Yellow
            , Block 1 0 Yellow
            , Block 0 1 Yellow
            , Block 1 1 Yellow
            ]

        SShape ->
            [ Block 1 0 Green
            , Block 2 0 Green
            , Block 0 1 Green
            , Block 1 1 Green
            ]

        TShape ->
            [ Block 1 0 Purple
            , Block 0 1 Purple
            , Block 1 1 Purple
            , Block 2 1 Purple
            ]

        ZShape ->
            [ Block 0 0 Red
            , Block 1 0 Red
            , Block 1 1 Red
            , Block 2 1 Red
            ]



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
        , lazy viewBlocks model.fallingPiece
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
