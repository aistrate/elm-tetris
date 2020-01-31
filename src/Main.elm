-- Copyright 2020, Adrian Istrate


module Main exposing (..)

import Block exposing (..)
import Browser
import Browser.Events
import Dict exposing (Dict)
import Json.Decode as Decode
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Lazy exposing (lazy, lazy2)
import Task
import Tetromino exposing (..)


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


type alias CellOccupancy =
    Dict ( Int, Int ) ()


type alias LockDelay =
    { timer : Maybe Float
    , movesRemaining : Int
    , maxRowReached : Int
    }


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
    , lockDelay : LockDelay
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
      , lockDelay = zeroLockDelay
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
            updateForMove
                (translateBy { dCol = 0, dRow = 1 })
                noAlternatives
                model

        MoveLeft ->
            updateForMove
                (translateBy { dCol = -1, dRow = 0 })
                noAlternatives
                model

        MoveRight ->
            updateForMove
                (translateBy { dCol = 1, dRow = 0 })
                noAlternatives
                model

        RotateClockwise ->
            updateForMove
                (Tetromino.rotate Clockwise)
                (wallKickAlternatives Clockwise)
                model

        RotateCounterclockwise ->
            updateForMove
                (Tetromino.rotate Counterclockwise)
                (wallKickAlternatives Counterclockwise)
                model

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
                interval DropAnimationInterval level

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
                |> centerHoriz game.columns
                |> translateBy
                    { dCol = 0
                    , dRow = spawningRow model.settings.level
                    }

        ( _, pieceBottomRow ) =
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
        , dropAnimationTimer = interval SpawningDropAnimationInterval model.settings.level
        , lockDelay =
            { timer = interval LockDelayInterval model.settings.level
            , movesRemaining = maxLockDelayMoves
            , maxRowReached = pieceBottomRow
            }
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
                (interval DropAnimationInterval model.settings.level)
                MoveDown

        ( lockDelayTimer, lockDelayCmd ) =
            updateTimer
                model.lockDelay.timer
                timeDelta
                Nothing
                LockToBottom

        ( fullRowsDelayTimer, fullRowsDelayCmd ) =
            updateTimer
                model.fullRowsDelayTimer
                timeDelta
                Nothing
                RemoveFullRows

        lockDelay =
            model.lockDelay
    in
    ( { model
        | dropAnimationTimer = dropAnimationTimer
        , lockDelay = { lockDelay | timer = lockDelayTimer }
        , fullRowsDelayTimer = fullRowsDelayTimer
      }
    , Cmd.batch
        [ dropAnimationCmd
        , lockDelayCmd
        , fullRowsDelayCmd
        ]
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
    = SpawningDropAnimationInterval
    | DropAnimationInterval
    | LockDelayInterval
    | FullRowsDelayInterval
    | CountdownInterval


interval : IntervalType -> Int -> Maybe Float
interval intervalType level =
    case intervalType of
        SpawningDropAnimationInterval ->
            if level == 0 then
                Nothing

            else
                Just 0

        DropAnimationInterval ->
            Dict.get level dropAnimationIntervals
                |> Maybe.withDefault Nothing

        LockDelayInterval ->
            Just 500

        FullRowsDelayInterval ->
            Just 200

        CountdownInterval ->
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


updateForMove : (Tetromino -> Tetromino) -> (Tetromino -> List Translation) -> Model -> ( Model, Cmd Msg )
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
                        ( _, pieceBottomRow ) =
                            rowRange movedPiece.blocks

                        lockDelay =
                            updateLockDelayAfterMove pieceBottomRow model.settings.level model.lockDelay
                    in
                    ( { model
                        | fallingPiece = Just movedPiece
                        , ghostPiece = calculateGhostPiece movedPiece.blocks model.occupiedCells
                        , lockDelay = lockDelay
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        Nothing ->
            ( model
            , Cmd.none
            )


updateLockDelayAfterMove : Int -> Int -> LockDelay -> LockDelay
updateLockDelayAfterMove pieceBottomRow level lockDelay =
    let
        maxRowReached =
            Basics.max pieceBottomRow lockDelay.maxRowReached

        ( movesRemaining, timer ) =
            if maxRowReached > lockDelay.maxRowReached then
                ( maxLockDelayMoves
                , interval LockDelayInterval level
                )

            else if lockDelay.movesRemaining > 0 then
                ( lockDelay.movesRemaining - 1
                , interval LockDelayInterval level
                )

            else if lockDelay.timer /= Nothing then
                ( 0
                , lockDelay.timer
                )

            else
                ( 0
                , Just 0
                )
    in
    { timer = timer
    , movesRemaining = movesRemaining
    , maxRowReached = maxRowReached
    }


firstViableAlternative : List Translation -> Tetromino -> CellOccupancy -> Maybe Tetromino
firstViableAlternative translations tetromino occupiedCells =
    case translations of
        translation :: remainingTranslations ->
            let
                alternative =
                    translateBy translation tetromino
            in
            if not (collision alternative.blocks occupiedCells) then
                Just alternative

            else
                firstViableAlternative remainingTranslations tetromino occupiedCells

        [] ->
            Nothing


calculateGhostPiece : List Block -> CellOccupancy -> List Block
calculateGhostPiece blocks occupiedCells =
    if not (List.isEmpty blocks) then
        let
            initial =
                List.map (\block -> { block | color = Gray }) blocks

            moveToBottomFrom : List Block -> List Block
            moveToBottomFrom current =
                let
                    next =
                        List.map (\block -> { block | row = block.row + 1 }) current
                in
                if collision next occupiedCells then
                    current

                else
                    moveToBottomFrom next
        in
        moveToBottomFrom initial

    else
        []


updateForDropAndLock : Model -> ( Model, Cmd Msg )
updateForDropAndLock model =
    case model.fallingPiece of
        Just fallingPiece ->
            let
                droppedPiece =
                    translateVertToTarget fallingPiece model.ghostPiece
            in
            ( { model
                | fallingPiece = Just droppedPiece
                , ghostPiece = calculateGhostPiece droppedPiece.blocks model.occupiedCells
                , lockDelay = zeroLockDelay
              }
            , Cmd.none
            )

        Nothing ->
            ( model
            , Cmd.none
            )


zeroLockDelay : LockDelay
zeroLockDelay =
    -- lock immediately (if on the bottom now, or moving there later)
    { timer = Just 0
    , movesRemaining = 0
    , maxRowReached = game.rows
    }


updateForLockToBottom : Model -> ( Model, Cmd Msg )
updateForLockToBottom model =
    case model.fallingPiece of
        Just fallingPiece ->
            let
                hasReachedBottom =
                    vertDistance fallingPiece.blocks model.ghostPiece == 0
            in
            if hasReachedBottom && model.lockDelay.timer == Nothing then
                let
                    bottomBlocks =
                        model.bottomBlocks ++ fallingPiece.blocks

                    hasFullRows =
                        not (List.isEmpty (fullRows game.columns bottomBlocks))

                    ( fullRowsDelayTimer, command ) =
                        if hasFullRows then
                            ( interval FullRowsDelayInterval model.settings.level
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
                    , occupiedCells = getOccupiedCells bottomBlocks
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
            removeFullRows game.columns model.bottomBlocks
    in
    ( { model
        | fallingPiece = Nothing
        , ghostPiece = []
        , bottomBlocks = bottomBlocks
        , occupiedCells = getOccupiedCells bottomBlocks
      }
    , triggerMessage NewShape
    )


getOccupiedCells : List Block -> CellOccupancy
getOccupiedCells bottomBlocks =
    List.map (\block -> ( ( block.col, block.row ), () )) bottomBlocks
        |> Dict.fromList


updateForRestart : Model -> ( Model, Cmd Msg )
updateForRestart model =
    let
        ( initModel, initCmd ) =
            init ()
    in
    ( { initModel
        | screen =
            CountdownScreen
                { timer = interval CountdownInterval model.settings.level
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
                { timer = interval CountdownInterval model.settings.level
                , afterCmd = Cmd.none
                }
      }
    , Cmd.none
    )


collision : List Block -> CellOccupancy -> Bool
collision blocks occupiedCells =
    let
        blockCollision block =
            not (0 <= block.col && block.col < game.columns && block.row < game.rows)
                || Dict.member ( block.col, block.row ) occupiedCells
    in
    List.any blockCollision blocks



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
            (\block -> 0 <= block.col && block.col < game.columns && 0 <= block.row && block.row < game.rows)
            blocks
            |> List.map (lazy viewBlock)
        )


viewBlock : Block -> Svg Msg
viewBlock block =
    rect
        [ x (String.fromFloat (toFloat block.col * blockStyle.size + blockStyle.borderWidth / 2))
        , y (String.fromFloat (toFloat block.row * blockStyle.size + blockStyle.borderWidth / 2))
        , width (String.fromFloat (blockStyle.size - blockStyle.borderWidth))
        , height (String.fromFloat (blockStyle.size - blockStyle.borderWidth))
        , fill (colorToHex block.color)
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
