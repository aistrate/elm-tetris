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
    = Block
        { col : Int
        , row : Int
        , color : Color
        }


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
    | CountdownScreen { timer : Maybe Float, afterCmd : Cmd Msg }
    | GameOverDialog
    | RestartDialog
    | PauseDialog
    | HelpDialog { returnScreen : Screen }


type alias Settings =
    { level : Int
    , showGhostPiece : Bool
    , showVerticalStripes : Bool
    }


type alias Model =
    { shapeBag : List Shape
    , fallingPiece : Maybe Tetromino
    , ghostPiece : List Block
    , bottomBlocks : List Block
    , occupiedCells : CellOccupancy
    , dropAnimationTimer : Maybe Float
    , lockDelayTimer : Maybe Float
    , lockDelayMovesRemaining : Int
    , maxRowReached : Int
    , fullRowsDelayTimer : Maybe Float
    , screen : Screen
    , settings : Settings
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { shapeBag = []
      , fallingPiece = Nothing
      , ghostPiece = []
      , bottomBlocks = []
      , occupiedCells = Dict.fromList []
      , dropAnimationTimer = Nothing
      , lockDelayTimer = Nothing
      , lockDelayMovesRemaining = 0
      , maxRowReached = 0
      , fullRowsDelayTimer = Nothing
      , screen = PlayScreen
      , settings =
            { level = 1
            , showGhostPiece = False
            , showVerticalStripes = False
            }
      }
    , triggerMessage NewShape
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
    = NewShape
    | ShapeBag (List Shape)
    | Spawn Shape
    | AnimationFrame Float
    | MoveDown
    | MoveLeft
    | MoveRight
    | RotateClockwise
    | RotateCounterclockwise
    | DropAndLock
    | LockToBottom
    | RemoveFullRows
    | ToggleGhostPiece
    | ToggleVerticalStripes
    | ShowRestartDialog
    | TogglePauseDialog
    | ToggleHelpDialog
    | AnswerYes
    | AnswerNo
    | Exit
    | Restart
    | Unpause
    | StopCountdown
    | LevelUp
    | LevelDown
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

                CountdownScreen { timer, afterCmd } ->
                    updateCountdownScreen timer afterCmd msg model

                GameOverDialog ->
                    updateGameOverDialog msg model

                RestartDialog ->
                    updateRestartDialog msg model

                PauseDialog ->
                    updatePauseDialog msg model

                HelpDialog { returnScreen } ->
                    updateHelpDialog returnScreen msg model


updatePlayScreen : Msg -> Model -> ( Model, Cmd Msg )
updatePlayScreen msg model =
    case msg of
        NewShape ->
            updateForNewShape model

        ShapeBag shapes ->
            ( { model | shapeBag = model.shapeBag ++ shapes }
            , triggerMessage NewShape
            )

        Spawn shape ->
            updateForSpawn shape model

        AnimationFrame timeDelta ->
            updateForAnimationFrame timeDelta model

        MoveDown ->
            updateForMove (shiftBy ( 0, 1 )) noAlternatives model

        MoveLeft ->
            updateForMove (shiftBy ( -1, 0 )) noAlternatives model

        MoveRight ->
            updateForMove (shiftBy ( 1, 0 )) noAlternatives model

        RotateClockwise ->
            updateForMove (rotate Clockwise) (wallKickAlternatives Clockwise) model

        RotateCounterclockwise ->
            updateForMove (rotate Counterclockwise) (wallKickAlternatives Counterclockwise) model

        DropAndLock ->
            updateForDropAndLock model

        LockToBottom ->
            updateForLockToBottom model

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

        Exit ->
            ( { model | screen = PauseDialog }
            , Cmd.none
            )

        ToggleHelpDialog ->
            ( { model | screen = HelpDialog { returnScreen = model.screen } }
            , Cmd.none
            )

        Restart ->
            updateForRestart model

        Unpause ->
            updateForUnpause model

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


updateCountdownScreen : Maybe Float -> Cmd Msg -> Msg -> Model -> ( Model, Cmd Msg )
updateCountdownScreen timer afterCmd msg model =
    case msg of
        AnimationFrame timeDelta ->
            case timer of
                Just _ ->
                    let
                        ( updatedTimer, cmd ) =
                            updateTimer
                                timer
                                timeDelta
                                Nothing
                                StopCountdown
                    in
                    ( { model
                        | screen =
                            CountdownScreen
                                { timer = updatedTimer
                                , afterCmd = afterCmd
                                }
                      }
                    , cmd
                    )

                Nothing ->
                    ( model
                    , triggerMessage StopCountdown
                    )

        StopCountdown ->
            ( { model | screen = PlayScreen }
            , afterCmd
            )

        _ ->
            ( model
            , Cmd.none
            )


updateGameOverDialog : Msg -> Model -> ( Model, Cmd Msg )
updateGameOverDialog msg model =
    case msg of
        AnswerYes ->
            ( { model | screen = PlayScreen }
            , triggerMessage Restart
            )

        ToggleHelpDialog ->
            ( { model | screen = HelpDialog { returnScreen = model.screen } }
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
            ( { model | screen = PlayScreen }
            , triggerMessage Restart
            )

        AnswerNo ->
            ( model
            , triggerMessage Exit
            )

        Exit ->
            ( { model | screen = PlayScreen }
            , triggerMessage Unpause
            )

        ToggleHelpDialog ->
            ( { model | screen = HelpDialog { returnScreen = model.screen } }
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
            ( model
            , triggerMessage Exit
            )

        Exit ->
            ( { model | screen = PlayScreen }
            , triggerMessage Unpause
            )

        ToggleHelpDialog ->
            ( { model | screen = HelpDialog { returnScreen = model.screen } }
            , Cmd.none
            )

        _ ->
            ( model
            , Cmd.none
            )


updateHelpDialog : Screen -> Msg -> Model -> ( Model, Cmd Msg )
updateHelpDialog returnScreen msg model =
    case msg of
        ToggleHelpDialog ->
            ( model
            , triggerMessage Exit
            )

        Exit ->
            ( { model | screen = returnScreen }
            , if returnScreen == PlayScreen then
                triggerMessage Unpause

              else
                Cmd.none
            )

        _ ->
            ( model
            , Cmd.none
            )


updateForNewShape : Model -> ( Model, Cmd Msg )
updateForNewShape model =
    case model.shapeBag of
        shape :: remainingShapes ->
            ( { model | shapeBag = remainingShapes }
            , triggerMessage (Spawn shape)
            )

        [] ->
            ( model
            , Random.generate ShapeBag shapeBagGenerator
            )


updateForSpawn : Shape -> Model -> ( Model, Cmd Msg )
updateForSpawn shape model =
    let
        spawnedPiece =
            spawnTetromino shape
                |> centerHoriz
                |> shiftBy ( 0, spawningRow model.settings.level )

        ( _, maxRowReached ) =
            rowRange spawnedPiece.blocks

        gameOver =
            collision spawnedPiece.blocks model.occupiedCells

        ( fallingPiece, ghostPiece, screen ) =
            if not gameOver then
                ( Just spawnedPiece
                , calculateGhostPiece spawnedPiece.blocks model.occupiedCells
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
        , lockDelayTimer = interval LockDelay model.settings.level
        , lockDelayMovesRemaining = maxLockDelayMoves
        , maxRowReached = maxRowReached
        , fullRowsDelayTimer = Nothing
        , screen = screen
      }
    , Cmd.none
    )


updateForAnimationFrame : Float -> Model -> ( Model, Cmd Msg )
updateForAnimationFrame timeDelta model =
    let
        ( dropAnimationTimer, dropAnimationCmd ) =
            updateTimer
                model.dropAnimationTimer
                timeDelta
                (interval DropAnimation model.settings.level)
                MoveDown

        ( lockDelayTimer, lockDelayCmd ) =
            updateTimer
                model.lockDelayTimer
                timeDelta
                Nothing
                LockToBottom

        ( fullRowsDelayTimer, fullRowsDelayCmd ) =
            updateTimer
                model.fullRowsDelayTimer
                timeDelta
                Nothing
                RemoveFullRows
    in
    ( { model
        | dropAnimationTimer = dropAnimationTimer
        , lockDelayTimer = lockDelayTimer
        , fullRowsDelayTimer = fullRowsDelayTimer
      }
    , Cmd.batch [ dropAnimationCmd, lockDelayCmd, fullRowsDelayCmd ]
    )


updateTimer : Maybe Float -> Float -> Maybe Float -> Msg -> ( Maybe Float, Cmd Msg )
updateTimer currentValue timeDelta resetValue message =
    case currentValue of
        Just value ->
            if value - timeDelta <= 0 then
                ( resetValue
                , triggerMessage message
                )

            else
                ( Just (value - timeDelta)
                , Cmd.none
                )

        Nothing ->
            ( Nothing
            , Cmd.none
            )


type IntervalType
    = DropAnimationOnSpawning
    | DropAnimation
    | LockDelay
    | FullRowsDelay
    | Countdown


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

        LockDelay ->
            Just 500

        FullRowsDelay ->
            Just 200

        Countdown ->
            Just 3000



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


maxLockDelayMoves : Int
maxLockDelayMoves =
    15


triggerMessage : Msg -> Cmd Msg
triggerMessage msg =
    Task.perform (always msg) (Task.succeed ())


updateForMove : (Tetromino -> Tetromino) -> (Tetromino -> List ( Int, Int )) -> Model -> ( Model, Cmd Msg )
updateForMove move alternativeTranslations model =
    case model.fallingPiece of
        Just fallingPiece ->
            let
                maybeMovedPiece =
                    firstViableAlternative
                        (alternativeTranslations fallingPiece)
                        (move fallingPiece)
                        model.occupiedCells
            in
            case maybeMovedPiece of
                Just movedPiece ->
                    let
                        ( _, bottomRow ) =
                            rowRange movedPiece.blocks

                        maxRowReached =
                            Basics.max bottomRow model.maxRowReached

                        ( lockDelayMovesRemaining, lockDelayTimer, command ) =
                            if maxRowReached > model.maxRowReached then
                                ( maxLockDelayMoves
                                , interval LockDelay model.settings.level
                                , Cmd.none
                                )

                            else if model.lockDelayMovesRemaining > 0 then
                                ( model.lockDelayMovesRemaining - 1
                                , interval LockDelay model.settings.level
                                , Cmd.none
                                )

                            else if model.lockDelayTimer /= Nothing then
                                ( 0
                                , model.lockDelayTimer
                                , Cmd.none
                                )

                            else
                                ( 0
                                , Nothing
                                , triggerMessage LockToBottom
                                )
                    in
                    ( { model
                        | fallingPiece = Just movedPiece
                        , ghostPiece = calculateGhostPiece movedPiece.blocks model.occupiedCells
                        , lockDelayTimer = lockDelayTimer
                        , lockDelayMovesRemaining = lockDelayMovesRemaining
                        , maxRowReached = maxRowReached
                      }
                    , command
                    )

                Nothing ->
                    ( model
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
                List.map (\(Block b) -> Block { b | color = Gray }) blocks

            moveToBottomFrom : List Block -> List Block
            moveToBottomFrom current =
                let
                    next =
                        List.map (\(Block b) -> Block { b | row = b.row + 1 }) current
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
            in
            ( { model
                | fallingPiece = Just droppedPiece
                , ghostPiece = calculateGhostPiece droppedPiece.blocks model.occupiedCells
                , lockDelayTimer = Nothing
              }
            , triggerMessage LockToBottom
            )

        Nothing ->
            ( model
            , Cmd.none
            )


updateForLockToBottom : Model -> ( Model, Cmd Msg )
updateForLockToBottom model =
    case model.fallingPiece of
        Just fallingPiece ->
            let
                hasReachedBottom =
                    vertDistance fallingPiece.blocks model.ghostPiece == 0
            in
            if hasReachedBottom && model.lockDelayTimer == Nothing then
                let
                    bottomBlocks =
                        model.bottomBlocks ++ fallingPiece.blocks

                    occupiedCells =
                        List.map (\(Block { col, row }) -> ( ( col, row ), () )) bottomBlocks
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
                            , triggerMessage NewShape
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

            else
                ( model
                , Cmd.none
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
            List.map (\(Block { col, row }) -> ( ( col, row ), () )) bottomBlocks
                |> Dict.fromList
    in
    ( { model
        | fallingPiece = Nothing
        , ghostPiece = []
        , bottomBlocks = bottomBlocks
        , occupiedCells = occupiedCells
      }
    , triggerMessage NewShape
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
                        List.filter (\(Block b) -> b.row == row) blocks
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
            List.filter (\(Block b) -> b.row /= row) blocks

        shiftBlock block =
            let
                (Block b) =
                    block
            in
            if b.row < row then
                Block { b | row = b.row + 1 }

            else
                block
    in
    List.map shiftBlock remainingBlocks


updateForRestart : Model -> ( Model, Cmd Msg )
updateForRestart model =
    let
        ( initModel, initCmd ) =
            init ()
    in
    ( { initModel
        | screen =
            CountdownScreen
                { timer = interval Countdown model.settings.level
                , afterCmd = initCmd
                }
        , settings = model.settings
      }
    , Cmd.none
    )


updateForUnpause : Model -> ( Model, Cmd Msg )
updateForUnpause model =
    ( { model
        | screen =
            CountdownScreen
                { timer = interval Countdown model.settings.level
                , afterCmd = Cmd.none
                }
      }
    , Cmd.none
    )


shapeBagGenerator : Random.Generator (List Shape)
shapeBagGenerator =
    shuffle [ IShape, JShape, LShape, OShape, SShape, TShape, ZShape ]


shuffle : List a -> Random.Generator (List a)
shuffle list =
    case list of
        [] ->
            Random.constant []

        head :: tail ->
            Random.uniform head tail
                |> Random.andThen
                    (\item ->
                        let
                            remainingItems =
                                List.filter (\i -> i /= item) list
                        in
                        Random.map2 (::)
                            (Random.constant item)
                            (shuffle remainingItems)
                    )


collision : List Block -> CellOccupancy -> Bool
collision blocks occupiedCells =
    let
        blockCollision (Block { col, row }) =
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
                (\(Block b) -> Block { b | col = b.col + colDelta, row = b.row + rowDelta })
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

        rotateBlock (Block b) =
            Block
                { b
                    | col = round (pivotCol - sign * (toFloat b.row - pivotRow))
                    , row = round (pivotRow + sign * (toFloat b.col - pivotCol))
                }
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
    range (\(Block { col }) -> col) blocks


rowRange : List Block -> ( Int, Int )
rowRange blocks =
    range (\(Block { row }) -> row) blocks


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
                [ Block { col = 0, row = 1, color = Cyan }
                , Block { col = 1, row = 1, color = Cyan }
                , Block { col = 2, row = 1, color = Cyan }
                , Block { col = 3, row = 1, color = Cyan }
                ]
            , pivot = ( 1.5, 1.5 )
            , shapeSize = Size4By1
            , rotationState = RotationState0
            }

        JShape ->
            { blocks =
                [ Block { col = 0, row = 0, color = Blue }
                , Block { col = 0, row = 1, color = Blue }
                , Block { col = 1, row = 1, color = Blue }
                , Block { col = 2, row = 1, color = Blue }
                ]
            , pivot = ( 1, 1 )
            , shapeSize = Size3By2
            , rotationState = RotationState0
            }

        LShape ->
            { blocks =
                [ Block { col = 2, row = 0, color = Orange }
                , Block { col = 0, row = 1, color = Orange }
                , Block { col = 1, row = 1, color = Orange }
                , Block { col = 2, row = 1, color = Orange }
                ]
            , pivot = ( 1, 1 )
            , shapeSize = Size3By2
            , rotationState = RotationState0
            }

        OShape ->
            { blocks =
                [ Block { col = 0, row = 0, color = Yellow }
                , Block { col = 1, row = 0, color = Yellow }
                , Block { col = 0, row = 1, color = Yellow }
                , Block { col = 1, row = 1, color = Yellow }
                ]
            , pivot = ( 0.5, 0.5 )
            , shapeSize = Size2By2
            , rotationState = RotationState0
            }

        SShape ->
            { blocks =
                [ Block { col = 1, row = 0, color = Green }
                , Block { col = 2, row = 0, color = Green }
                , Block { col = 0, row = 1, color = Green }
                , Block { col = 1, row = 1, color = Green }
                ]
            , pivot = ( 1, 1 )
            , shapeSize = Size3By2
            , rotationState = RotationState0
            }

        TShape ->
            { blocks =
                [ Block { col = 1, row = 0, color = Purple }
                , Block { col = 0, row = 1, color = Purple }
                , Block { col = 1, row = 1, color = Purple }
                , Block { col = 2, row = 1, color = Purple }
                ]
            , pivot = ( 1, 1 )
            , shapeSize = Size3By2
            , rotationState = RotationState0
            }

        ZShape ->
            { blocks =
                [ Block { col = 0, row = 0, color = Red }
                , Block { col = 1, row = 0, color = Red }
                , Block { col = 1, row = 1, color = Red }
                , Block { col = 2, row = 1, color = Red }
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
        , case model.screen of
            PlayScreen ->
                Browser.Events.onAnimationFrameDelta AnimationFrame

            CountdownScreen _ ->
                Browser.Events.onAnimationFrameDelta AnimationFrame

            _ ->
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
            Exit

        -- IE
        "esc" ->
            Exit

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
    g
        []
        (List.filter
            (\(Block { col, row }) ->
                0 <= col && col < game.columns && 0 <= row && row < game.rows
            )
            blocks
            |> List.map (lazy viewBlock)
        )


viewBlock : Block -> Svg Msg
viewBlock (Block b) =
    rect
        [ x (String.fromFloat (toFloat b.col * blockStyle.size + blockStyle.borderWidth / 2))
        , y (String.fromFloat (toFloat b.row * blockStyle.size + blockStyle.borderWidth / 2))
        , width (String.fromFloat (blockStyle.size - blockStyle.borderWidth))
        , height (String.fromFloat (blockStyle.size - blockStyle.borderWidth))
        , fill (colorToHex b.color)
        , stroke "white"
        , strokeWidth (String.fromFloat blockStyle.borderWidth)
        ]
        []


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

        CountdownScreen { timer } ->
            viewCountdownScreen timer

        GameOverDialog ->
            viewGameOverDialog

        RestartDialog ->
            viewRestartDialog

        PauseDialog ->
            viewPauseDialog

        HelpDialog _ ->
            viewHelpDialog


viewCountdownScreen : Maybe Float -> Svg Msg
viewCountdownScreen timer =
    let
        countdown =
            ceiling (Maybe.withDefault 0 timer / 1000)
    in
    g
        []
        [ viewDialogOverlay
        , lazy viewCountdownText countdown
        ]


viewCountdownText : Int -> Svg Msg
viewCountdownText countdown =
    text_
        []
        [ tspan
            [ x (String.fromFloat middleBoardX)
            , y (String.fromFloat (toFloat game.rows * blockStyle.size / 2))
            , textAnchor "middle"
            , dominantBaseline "middle"
            , fontSize (String.fromFloat (blockStyle.size * 3))
            , fontWeight "bold"
            ]
            [ text
                (if countdown > 0 then
                    String.fromInt countdown

                 else
                    " "
                )
            ]
        ]


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
        , LargeText "Press Esc or P to continue"
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
        , Shortcut "Esc or P" "Pause"
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
                [ x (String.fromFloat middleBoardX)
                , yCoord
                , textAnchor "middle"
                , fontSize (String.fromFloat (blockStyle.size * 0.65))
                , fontWeight "bold"
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


middleBoardX : Float
middleBoardX =
    toFloat game.columns * blockStyle.size / 2


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
