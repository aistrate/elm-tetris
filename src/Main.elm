-- Copyright 2020, Adrian Istrate


module Main exposing (..)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Json.Decode as Decode
import Random
import Svg exposing (..)
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
    , padding = 1.5
    , footerHeight = 100.0
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
    -- See https://tetris.wiki/Super_Rotation_System
    RotationState
    = RotationState0 -- spawn state ("upper" horizontal, flat side down)
    | RotationStateR -- state resulting from a clockwise rotation ("right") from spawn
    | RotationState2 -- state resulting from 2 successive rotations in either direction from spawn
    | RotationStateL -- state resulting from a counter-clockwise ("left") rotation from spawn


type alias Tetromino =
    { blocks : List Block
    , pivot : ( Float, Float )
    , shapeSize : ShapeSize
    , rotationState : RotationState
    }


type alias CellOccupancy =
    Dict ( Int, Int ) ()


type Screen
    = PlayScreen
    | GameOverDialog
    | RestartDialog
    | PauseDialog
    | HelpDialog Screen


type alias Settings =
    { level : Int
    , showGhostPiece : Bool
    , showVerticalStripes : Bool
    }


type alias Model =
    { fallingPiece : Maybe Tetromino
    , ghostPiece : List Block
    , bottomBlocks : List Block
    , occupiedCells : CellOccupancy
    , dropAnimationTimer : Maybe Float
    , fullRowsDelayTimer : Maybe Float
    , screen : Screen
    , settings : Settings
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { fallingPiece = Nothing
      , ghostPiece = []
      , bottomBlocks = []
      , occupiedCells = Dict.fromList []
      , dropAnimationTimer = Nothing
      , fullRowsDelayTimer = Nothing
      , screen = PlayScreen
      , settings =
            { level = 1
            , showGhostPiece = False
            , showVerticalStripes = False
            }
      }
    , generateShape
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
    = Spawn Shape
    | AnimationFrame Float
    | MoveLeft
    | MoveRight
    | MoveDown
    | RotateClockwise
    | RotateCounterclockwise
    | DropAndLock
    | RemoveFullRows
    | ShowRestartDialog
    | TogglePauseDialog
    | ToggleHelpDialog
    | AnswerYes
    | AnswerNo
    | ExitDialog
    | LevelUp
    | LevelDown
    | ToggleGhostPiece
    | ToggleVerticalStripes
    | OtherKey


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LevelUp ->
            updateForLevelChange (model.settings.level + 1) model

        LevelDown ->
            updateForLevelChange (model.settings.level - 1) model

        _ ->
            case model.screen of
                PlayScreen ->
                    updatePlayScreen msg model

                GameOverDialog ->
                    updateGameOverDialog msg model

                RestartDialog ->
                    updateRestartDialog msg model

                PauseDialog ->
                    updatePauseDialog msg model

                HelpDialog prevScreen ->
                    updateHelpDialog prevScreen msg model


updatePlayScreen : Msg -> Model -> ( Model, Cmd Msg )
updatePlayScreen msg model =
    case msg of
        Spawn shape ->
            updateForSpawn shape model

        AnimationFrame timeDelta ->
            updateForAnimationFrame timeDelta model

        MoveLeft ->
            updateForTransform (shiftBy ( -1, 0 )) noAlternatives model

        MoveRight ->
            updateForTransform (shiftBy ( 1, 0 )) noAlternatives model

        MoveDown ->
            updateForTransform (shiftBy ( 0, 1 )) noAlternatives model

        RotateClockwise ->
            updateForTransform (rotate Clockwise) (wallKickAlternatives Clockwise) model

        RotateCounterclockwise ->
            updateForTransform (rotate Counterclockwise) (wallKickAlternatives Counterclockwise) model

        DropAndLock ->
            updateForDropAndLock model

        RemoveFullRows ->
            updateForRemoveFullRows model

        ShowRestartDialog ->
            ( { model | screen = RestartDialog }
            , Cmd.none
            )

        TogglePauseDialog ->
            ( { model | screen = PauseDialog }
            , Cmd.none
            )

        ToggleHelpDialog ->
            ( { model | screen = HelpDialog model.screen }
            , Cmd.none
            )

        ToggleGhostPiece ->
            let
                settings =
                    model.settings
            in
            ( { model | settings = { settings | showGhostPiece = not settings.showGhostPiece } }
            , Cmd.none
            )

        ToggleVerticalStripes ->
            let
                settings =
                    model.settings
            in
            ( { model | settings = { settings | showVerticalStripes = not settings.showVerticalStripes } }
            , Cmd.none
            )

        _ ->
            ( model
            , Cmd.none
            )


updateForLevelChange : Int -> Model -> ( Model, Cmd Msg )
updateForLevelChange level model =
    let
        settings =
            model.settings

        dropAnimationTimer =
            if
                settings.level
                    == 0
                    && level
                    == 1
                    && model.dropAnimationTimer
                    == Nothing
                    && model.fallingPiece
                    /= Nothing
            then
                interval DropAnimation level

            else
                model.dropAnimationTimer
    in
    ( { model
        | settings = { settings | level = clamp 0 maxLevel level }
        , dropAnimationTimer = dropAnimationTimer
      }
    , Cmd.none
    )


updateGameOverDialog : Msg -> Model -> ( Model, Cmd Msg )
updateGameOverDialog msg model =
    case msg of
        AnswerYes ->
            let
                ( initModel, initCmd ) =
                    init ()
            in
            ( { initModel | settings = model.settings }
            , initCmd
            )

        ToggleHelpDialog ->
            ( { model | screen = HelpDialog model.screen }
            , Cmd.none
            )

        _ ->
            ( model
            , Cmd.none
            )


updateRestartDialog : Msg -> Model -> ( Model, Cmd Msg )
updateRestartDialog msg model =
    case msg of
        AnswerYes ->
            let
                ( initModel, initCmd ) =
                    init ()
            in
            ( { initModel | settings = model.settings }
            , initCmd
            )

        AnswerNo ->
            ( { model | screen = PlayScreen }
            , Cmd.none
            )

        ExitDialog ->
            ( { model | screen = PlayScreen }
            , Cmd.none
            )

        ToggleHelpDialog ->
            ( { model | screen = HelpDialog model.screen }
            , Cmd.none
            )

        _ ->
            ( model
            , Cmd.none
            )


updatePauseDialog : Msg -> Model -> ( Model, Cmd Msg )
updatePauseDialog msg model =
    case msg of
        TogglePauseDialog ->
            ( { model | screen = PlayScreen }
            , Cmd.none
            )

        ExitDialog ->
            ( { model | screen = PlayScreen }
            , Cmd.none
            )

        ToggleHelpDialog ->
            ( { model | screen = HelpDialog model.screen }
            , Cmd.none
            )

        _ ->
            ( model
            , Cmd.none
            )


updateHelpDialog : Screen -> Msg -> Model -> ( Model, Cmd Msg )
updateHelpDialog prevScreen msg model =
    case msg of
        ToggleHelpDialog ->
            ( { model | screen = prevScreen }
            , Cmd.none
            )

        ExitDialog ->
            ( { model | screen = prevScreen }
            , Cmd.none
            )

        _ ->
            ( model
            , Cmd.none
            )


updateForSpawn : Shape -> Model -> ( Model, Cmd Msg )
updateForSpawn shape model =
    let
        spawnedFallingPiece =
            spawnTetromino shape
                |> centerHoriz
                |> shiftBy ( 0, spawningRow model.settings.level )

        gameOver =
            collision spawnedFallingPiece.blocks model.occupiedCells

        ( fallingPiece, ghostPiece, screen ) =
            if not gameOver then
                ( Just spawnedFallingPiece
                , calculateGhostPiece spawnedFallingPiece.blocks model.occupiedCells
                , PlayScreen
                )

            else
                ( Nothing
                , []
                , GameOverDialog
                )
    in
    ( { model
        | fallingPiece = fallingPiece
        , ghostPiece = ghostPiece
        , dropAnimationTimer = interval DropAnimationOnSpawning model.settings.level
        , fullRowsDelayTimer = Nothing
        , screen = screen
      }
    , Cmd.none
    )


updateForAnimationFrame : Float -> Model -> ( Model, Cmd Msg )
updateForAnimationFrame timeDelta model =
    case model.fullRowsDelayTimer of
        Just fullRowsDelayTimer ->
            if fullRowsDelayTimer - timeDelta <= 0 then
                ( { model | fullRowsDelayTimer = Nothing }
                , triggerMessage RemoveFullRows
                )

            else
                ( { model | fullRowsDelayTimer = Just (fullRowsDelayTimer - timeDelta) }
                , Cmd.none
                )

        Nothing ->
            case model.dropAnimationTimer of
                Just dropAnimationTimer ->
                    if dropAnimationTimer - timeDelta <= 0 then
                        ( { model | dropAnimationTimer = interval DropAnimation model.settings.level }
                        , triggerMessage MoveDown
                        )

                    else
                        ( { model | dropAnimationTimer = Just (dropAnimationTimer - timeDelta) }
                        , Cmd.none
                        )

                Nothing ->
                    ( model
                    , Cmd.none
                    )


type IntervalType
    = DropAnimationOnSpawning
    | DropAnimation
    | FullRowsDelay


interval : IntervalType -> Int -> Maybe Float
interval intervalType level =
    case intervalType of
        DropAnimationOnSpawning ->
            if level == 0 then
                Nothing

            else
                Just 0

        DropAnimation ->
            Dict.get level dropAnimationIntervals
                |> Maybe.withDefault Nothing

        FullRowsDelay ->
            Just 200



-- See https://tetris.fandom.com/wiki/Tetris_Worlds, under the heading Gravity.
-- Formula: round (1000 * pow (0.8 - ((toFloat level - 1) * 0.007)) (level - 1)),
-- where (pow x n) is x to the nth power


dropAnimationIntervals : Dict Int (Maybe Float)
dropAnimationIntervals =
    Dict.fromList
        [ ( 0, Nothing )
        , ( 1, Just 1000 )
        , ( 2, Just 793 )
        , ( 3, Just 618 )
        , ( 4, Just 473 )
        , ( 5, Just 355 )
        , ( 6, Just 262 )
        , ( 7, Just 190 )
        , ( 8, Just 135 )
        , ( 9, Just 94 )
        , ( 10, Just 64 )
        , ( 11, Just 43 )
        , ( 12, Just 28 )
        ]


maxLevel : Int
maxLevel =
    12


spawningRow : Int -> Int
spawningRow level =
    if level == 0 then
        0

    else
        -2


triggerMessage : Msg -> Cmd Msg
triggerMessage msg =
    Task.perform (always msg) (Task.succeed ())


updateForTransform : (Tetromino -> Tetromino) -> (Tetromino -> List ( Int, Int )) -> Model -> ( Model, Cmd Msg )
updateForTransform transform alternativeTranslations model =
    case model.fallingPiece of
        Just fallingPiece ->
            let
                viableFallingPiece =
                    firstViableAlternative
                        (alternativeTranslations fallingPiece)
                        (transform fallingPiece)
                        model.occupiedCells
                        |> Maybe.withDefault fallingPiece
            in
            ( { model
                | fallingPiece = Just viableFallingPiece
                , ghostPiece = calculateGhostPiece viableFallingPiece.blocks model.occupiedCells
              }
            , Cmd.none
            )

        Nothing ->
            ( model
            , Cmd.none
            )


firstViableAlternative : List ( Int, Int ) -> Tetromino -> CellOccupancy -> Maybe Tetromino
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



-- See https://tetris.wiki/Super_Rotation_System for details on tetromino Wall Kicks after rotation


wallKickAlternatives : RotationDirection -> Tetromino -> List ( Int, Int )
wallKickAlternatives direction tetromino =
    let
        alternatives =
            case ( tetromino.shapeSize, tetromino.rotationState, direction ) of
                --
                -- Size3By2 (JShape, LShape, SShape, TShape, ZShape)
                ( Size3By2, RotationState0, Clockwise ) ->
                    [ ( 0, 0 ), ( -1, 0 ), ( -1, 1 ), ( 0, -2 ), ( -1, -2 ) ]

                ( Size3By2, RotationStateR, Counterclockwise ) ->
                    [ ( 0, 0 ), ( 1, 0 ), ( 1, -1 ), ( 0, 2 ), ( 1, 2 ) ]

                ( Size3By2, RotationStateR, Clockwise ) ->
                    [ ( 0, 0 ), ( 1, 0 ), ( 1, -1 ), ( 0, 2 ), ( 1, 2 ) ]

                ( Size3By2, RotationState2, Counterclockwise ) ->
                    [ ( 0, 0 ), ( -1, 0 ), ( -1, 1 ), ( 0, -2 ), ( -1, -2 ) ]

                ( Size3By2, RotationState2, Clockwise ) ->
                    [ ( 0, 0 ), ( 1, 0 ), ( 1, 1 ), ( 0, -2 ), ( 1, -2 ) ]

                ( Size3By2, RotationStateL, Counterclockwise ) ->
                    [ ( 0, 0 ), ( -1, 0 ), ( -1, -1 ), ( 0, 2 ), ( -1, 2 ) ]

                ( Size3By2, RotationStateL, Clockwise ) ->
                    [ ( 0, 0 ), ( -1, 0 ), ( -1, -1 ), ( 0, 2 ), ( -1, 2 ) ]

                ( Size3By2, RotationState0, Counterclockwise ) ->
                    [ ( 0, 0 ), ( 1, 0 ), ( 1, 1 ), ( 0, -2 ), ( 1, -2 ) ]

                --
                -- Size4By1 (IShape)
                ( Size4By1, RotationState0, Clockwise ) ->
                    [ ( 0, 0 ), ( -2, 0 ), ( 1, 0 ), ( -2, -1 ), ( 1, 2 ) ]

                ( Size4By1, RotationStateR, Counterclockwise ) ->
                    [ ( 0, 0 ), ( 2, 0 ), ( -1, 0 ), ( 2, 1 ), ( -1, -2 ) ]

                ( Size4By1, RotationStateR, Clockwise ) ->
                    [ ( 0, 0 ), ( -1, 0 ), ( 2, 0 ), ( -1, 2 ), ( 2, -1 ) ]

                ( Size4By1, RotationState2, Counterclockwise ) ->
                    [ ( 0, 0 ), ( 1, 0 ), ( -2, 0 ), ( 1, -2 ), ( -2, 1 ) ]

                ( Size4By1, RotationState2, Clockwise ) ->
                    [ ( 0, 0 ), ( 2, 0 ), ( -1, 0 ), ( 2, 1 ), ( -1, -2 ) ]

                ( Size4By1, RotationStateL, Counterclockwise ) ->
                    [ ( 0, 0 ), ( -2, 0 ), ( 1, 0 ), ( -2, -1 ), ( 1, 2 ) ]

                ( Size4By1, RotationStateL, Clockwise ) ->
                    [ ( 0, 0 ), ( 1, 0 ), ( -2, 0 ), ( 1, -2 ), ( -2, 1 ) ]

                ( Size4By1, RotationState0, Counterclockwise ) ->
                    [ ( 0, 0 ), ( -1, 0 ), ( 2, 0 ), ( -1, 2 ), ( 2, -1 ) ]

                --
                -- Size2By2 (OShape)
                ( Size2By2, _, _ ) ->
                    [ ( 0, 0 ) ]
    in
    List.map (\( col, row ) -> ( col, -row )) alternatives


calculateGhostPiece : List Block -> CellOccupancy -> List Block
calculateGhostPiece blocks occupiedCells =
    if not (List.isEmpty blocks) then
        let
            initial =
                List.map (\(Block col row _) -> Block col row Gray) blocks

            moveToBottomFrom : List Block -> List Block
            moveToBottomFrom current =
                let
                    next =
                        List.map (\(Block col row color) -> Block col (row + 1) color) current
                in
                if collision next occupiedCells then
                    current

                else
                    moveToBottomFrom next
        in
        moveToBottomFrom initial

    else
        []


shiftVertToTarget : Tetromino -> List Block -> Tetromino
shiftVertToTarget tetromino target =
    let
        rowDelta =
            vertDistance tetromino.blocks target
    in
    shiftBy ( 0, rowDelta ) tetromino


vertDistance : List Block -> List Block -> Int
vertDistance source dest =
    let
        ( sourceMinRow, _ ) =
            rowRange source

        ( destMinRow, _ ) =
            rowRange dest
    in
    destMinRow - sourceMinRow


updateForDropAndLock : Model -> ( Model, Cmd Msg )
updateForDropAndLock model =
    case model.fallingPiece of
        Just fallingPiece ->
            let
                droppedPiece =
                    shiftVertToTarget fallingPiece model.ghostPiece

                bottomBlocks =
                    model.bottomBlocks ++ droppedPiece.blocks

                occupiedCells =
                    List.map (\(Block col row _) -> ( ( col, row ), () )) bottomBlocks
                        |> Dict.fromList

                hasFullRows =
                    not (List.isEmpty (fullRows bottomBlocks))

                ( fullRowsDelayTimer, command ) =
                    if hasFullRows then
                        ( interval FullRowsDelay model.settings.level
                        , Cmd.none
                        )

                    else
                        ( Nothing
                        , generateShape
                        )
            in
            ( { model
                | fallingPiece = Nothing
                , ghostPiece = []
                , bottomBlocks = bottomBlocks
                , occupiedCells = occupiedCells
                , dropAnimationTimer = Nothing
                , fullRowsDelayTimer = fullRowsDelayTimer
              }
            , command
            )

        Nothing ->
            ( model
            , Cmd.none
            )


updateForRemoveFullRows : Model -> ( Model, Cmd Msg )
updateForRemoveFullRows model =
    let
        bottomBlocks =
            removeFullRows model.bottomBlocks

        occupiedCells =
            List.map (\(Block col row _) -> ( ( col, row ), () )) bottomBlocks
                |> Dict.fromList
    in
    ( { model
        | fallingPiece = Nothing
        , ghostPiece = []
        , bottomBlocks = bottomBlocks
        , occupiedCells = occupiedCells
      }
    , generateShape
    )


fullRows : List Block -> List Int
fullRows blocks =
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
    in
    List.filter (\( _, count ) -> count == game.columns) rowCounts
        |> List.map (\( row, _ ) -> row)


removeFullRows : List Block -> List Block
removeFullRows blocks =
    List.foldl removeRow blocks (fullRows blocks)


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


generateShape : Cmd Msg
generateShape =
    Random.generate Spawn shapeGenerator


shapeGenerator : Random.Generator Shape
shapeGenerator =
    Random.uniform IShape [ JShape, LShape, OShape, SShape, TShape, ZShape ]


collision : List Block -> CellOccupancy -> Bool
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


nextRotationState : RotationState -> RotationDirection -> RotationState
nextRotationState currentRotationState direction =
    case ( currentRotationState, direction ) of
        ( RotationState0, Clockwise ) ->
            RotationStateR

        ( RotationStateR, Clockwise ) ->
            RotationState2

        ( RotationState2, Clockwise ) ->
            RotationStateL

        ( RotationStateL, Clockwise ) ->
            RotationState0

        ( RotationState0, Counterclockwise ) ->
            RotationStateL

        ( RotationStateL, Counterclockwise ) ->
            RotationState2

        ( RotationState2, Counterclockwise ) ->
            RotationStateR

        ( RotationStateR, Counterclockwise ) ->
            RotationState0



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown keyDecoder
        , if model.screen == PlayScreen then
            Browser.Events.onAnimationFrameDelta AnimationFrame

          else
            Sub.none
        ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKeyboardMsg (Decode.field "key" Decode.string)


toKeyboardMsg : String -> Msg
toKeyboardMsg key =
    case String.toLower key of
        "arrowleft" ->
            MoveLeft

        -- IE
        "left" ->
            MoveLeft

        "arrowright" ->
            MoveRight

        -- IE
        "right" ->
            MoveRight

        "arrowdown" ->
            MoveDown

        -- IE
        "down" ->
            MoveDown

        "arrowup" ->
            RotateClockwise

        -- IE
        "up" ->
            RotateClockwise

        "x" ->
            RotateClockwise

        "control" ->
            RotateCounterclockwise

        "z" ->
            RotateCounterclockwise

        " " ->
            DropAndLock

        -- IE
        "spacebar" ->
            DropAndLock

        "r" ->
            ShowRestartDialog

        "p" ->
            TogglePauseDialog

        "h" ->
            ToggleHelpDialog

        "y" ->
            AnswerYes

        "n" ->
            AnswerNo

        "escape" ->
            ExitDialog

        -- IE
        "esc" ->
            ExitDialog

        "+" ->
            LevelUp

        "-" ->
            LevelDown

        "g" ->
            ToggleGhostPiece

        "v" ->
            ToggleVerticalStripes

        _ ->
            OtherKey



-- VIEW


view : Model -> Svg Msg
view model =
    svg
        rootSvgAttributes
        [ lazy viewFooter model.settings.level
        , viewBoard
        , lazy viewVerticalStripes model.settings.showVerticalStripes
        , lazy viewBlocks model.bottomBlocks
        , lazy2 viewGhostPiece model.settings.showGhostPiece model.ghostPiece
        , lazy viewFallingPiece model.fallingPiece
        , lazy viewDialogIfAny model.screen
        ]


boardWidth : Float
boardWidth =
    boardSize game.columns


boardHeight : Float
boardHeight =
    boardSize game.rows


boardSize : Int -> Float
boardSize blockCount =
    toFloat blockCount * blockStyle.size + 2 * (boardStyle.borderWidth + boardStyle.padding)


rootSvgAttributes : List (Attribute Msg)
rootSvgAttributes =
    [ width "100%"
    , height (String.fromFloat (boardStyle.marginTop + boardHeight + boardStyle.footerHeight))
    , viewBox
        (String.fromFloat -(boardStyle.borderWidth + boardStyle.padding)
            ++ " "
            ++ String.fromFloat -(boardStyle.marginTop + boardStyle.borderWidth + boardStyle.padding)
            ++ " "
            ++ String.fromFloat boardWidth
            ++ " "
            ++ String.fromFloat (boardStyle.marginTop + boardHeight + boardStyle.footerHeight)
        )
    , fontFamily "sans-serif"
    , fill "#222"
    ]


viewBoard : Svg Msg
viewBoard =
    rect
        [ x (String.fromFloat -(boardStyle.borderWidth / 2 + boardStyle.padding))
        , y (String.fromFloat -(boardStyle.borderWidth / 2 + boardStyle.padding))
        , width (String.fromFloat (boardWidth - boardStyle.borderWidth))
        , height (String.fromFloat (boardHeight - boardStyle.borderWidth))
        , fill "transparent"
        , stroke "#D3BCA3"
        , strokeWidth (String.fromFloat boardStyle.borderWidth)
        ]
        []


viewBlocks : List Block -> Svg Msg
viewBlocks blocks =
    let
        viewBlock (Block col row color) =
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
    in
    g
        []
        (List.filter
            (\(Block col row color) ->
                0 <= col && col < game.columns && 0 <= row && row < game.rows
            )
            blocks
            |> List.map viewBlock
        )


viewGhostPiece : Bool -> List Block -> Svg Msg
viewGhostPiece visible blocks =
    if visible then
        viewBlocks blocks

    else
        g [] []


viewFallingPiece : Maybe Tetromino -> Svg Msg
viewFallingPiece fallingPiece =
    let
        blocks =
            Maybe.map .blocks fallingPiece
                |> Maybe.withDefault []
    in
    viewBlocks blocks


viewVerticalStripes : Bool -> Svg Msg
viewVerticalStripes visible =
    let
        viewVerticalStripe col =
            rect
                [ x (String.fromFloat (toFloat col * blockStyle.size + blockStyle.borderWidth / 2))
                , y "0"
                , width (String.fromFloat (blockStyle.size - blockStyle.borderWidth))
                , height (String.fromFloat (toFloat game.rows * blockStyle.size - blockStyle.borderWidth))
                , fill "#F4EFE9"
                , stroke "white"
                , strokeWidth (String.fromFloat blockStyle.borderWidth)
                ]
                []
    in
    g
        []
        (if visible then
            List.range 0 (game.columns - 1)
                |> List.filter (\col -> modBy 2 col == 1)
                |> List.map viewVerticalStripe

         else
            []
        )


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


viewDialogIfAny : Screen -> Svg Msg
viewDialogIfAny screen =
    case screen of
        PlayScreen ->
            g [] []

        GameOverDialog ->
            viewGameOverDialog

        RestartDialog ->
            viewRestartDialog

        PauseDialog ->
            viewPauseDialog

        HelpDialog _ ->
            viewHelpDialog


type DialogTextLine
    = LargeText String
    | Shortcut String String
    | EmptyLine


viewGameOverDialog : Svg Msg
viewGameOverDialog =
    viewDialog
        [ LargeText "Game Over"
        , EmptyLine
        , LargeText "Restart? (Y/N)"
        ]


viewRestartDialog : Svg Msg
viewRestartDialog =
    viewDialog
        [ LargeText "Restart? (Y/N)"
        ]


viewPauseDialog : Svg Msg
viewPauseDialog =
    viewDialog
        [ LargeText "Paused"
        , EmptyLine
        , LargeText "Press P to continue"
        ]


viewHelpDialog : Svg Msg
viewHelpDialog =
    viewDialog
        [ Shortcut "Arrow Left" "Move left"
        , Shortcut "Arrow Right" "Move right"
        , Shortcut "Arrow Down" "Move down"
        , EmptyLine
        , Shortcut "Arrow Up or X" "Rotate clockwise"
        , Shortcut "Ctrl or Z" "Rotate counterclockwise"
        , EmptyLine
        , Shortcut "Space" "Drop"
        , EmptyLine
        , Shortcut "P" "Pause"
        , Shortcut "R" "Restart"
        , EmptyLine
        , Shortcut "+" ("Level up (0 - " ++ String.fromInt maxLevel ++ ")")
        , Shortcut "-" "Level down"
        , EmptyLine
        , Shortcut "G" "Ghost piece"
        , Shortcut "V" "Vertical stripes"
        , EmptyLine
        , LargeText "Press Esc or H to exit"
        ]


viewDialog : List DialogTextLine -> Svg Msg
viewDialog textLines =
    let
        vertCenteredFirstRow =
            (game.rows - List.length textLines) // 2

        firstLineY =
            String.fromFloat ((toFloat vertCenteredFirstRow + 0.8) * blockStyle.size)

        nextLineDy =
            String.fromFloat blockStyle.size
    in
    g
        []
        [ viewDialogOverlay
        , text_
            []
            (List.indexedMap
                (\idx textLine ->
                    let
                        yCoord =
                            if idx == 0 then
                                y firstLineY

                            else
                                dy nextLineDy
                    in
                    viewDialogTextLine yCoord textLine
                )
                textLines
            )
        ]


viewDialogTextLine : Attribute Msg -> DialogTextLine -> Svg Msg
viewDialogTextLine yCoord textLine =
    case textLine of
        LargeText largeText ->
            tspan
                [ x "50%"
                , yCoord
                , fontSize (String.fromFloat (blockStyle.size * 0.65))
                , fontWeight "bold"
                , textAnchor "middle"
                ]
                [ text largeText
                ]

        Shortcut key description ->
            tspan
                [ yCoord
                , fontSize (String.fromFloat (blockStyle.size * 0.5))
                ]
                [ tspan
                    [ x (String.fromFloat (blockStyle.size * 0.25))
                    ]
                    [ text key
                    ]
                , tspan
                    [ x (String.fromFloat (blockStyle.size * 4.25))
                    ]
                    [ text description
                    ]
                ]

        EmptyLine ->
            tspan
                [ x "0"
                , yCoord
                , fontSize "10"
                ]
                [ text " "
                ]


viewDialogOverlay : Svg Msg
viewDialogOverlay =
    rect
        [ x (String.fromFloat -(boardStyle.borderWidth + boardStyle.padding))
        , y (String.fromFloat -(boardStyle.borderWidth + boardStyle.padding))
        , width (String.fromFloat boardWidth)
        , height (String.fromFloat boardHeight)
        , fill "white"
        , opacity "0.7"
        ]
        []


viewFooter : Int -> Svg Msg
viewFooter level =
    let
        footerY =
            boardHeight - (boardStyle.borderWidth + boardStyle.padding)

        levelText =
            "Level "
                ++ String.fromInt level
                ++ (if level == 0 then
                        " (no gravity)"

                    else
                        ""
                   )
    in
    g
        []
        [ text_
            []
            [ tspan
                [ x "0"
                , y (String.fromFloat (footerY + blockStyle.size))
                , fontSize (String.fromFloat (blockStyle.size * 0.65))
                ]
                [ text levelText
                ]
            , tspan
                [ y (String.fromFloat (footerY + boardStyle.footerHeight - 4))
                , fontSize "15"
                ]
                [ tspan
                    [ x "0"
                    ]
                    [ text "Press H for Help"
                    ]
                , a
                    [ xlinkHref "https://github.com/aistrate/elm-tetris"
                    , target "_blank"
                    , Svg.Attributes.style "fill: #0366D6; text-decoration: underline;"
                    ]
                    [ tspan
                        [ x (String.fromFloat (blockStyle.size * toFloat game.columns))
                        , textAnchor "end"
                        ]
                        [ text "Code"
                        ]
                    ]
                ]
            ]
        ]
