module PlayScreen exposing (..)

import Block exposing (..)
import Board exposing (..)
import Common exposing (..)
import Dialogs exposing (..)
import Dict exposing (Dict)
import Shape exposing (..)
import SidePanel exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Lazy exposing (..)
import Tetromino exposing (..)
import Timer exposing (..)



-- MODEL


type alias Model =
    { fallingPiece : Maybe Tetromino
    , ghostPiece : List Block
    , bottomBlocks : List Block
    , board : Board
    , dropAnimationTimer : TimeInterval
    , lockDelay : LockDelay
    , fullRowsDelayTimer : TimeInterval
    , screen : Screen
    , sidePanel : SidePanel
    , settings : Settings
    }


type alias LockDelay =
    { timer : TimeInterval
    , movesRemaining : Int
    , maxRowReached : Int
    }


type alias Settings =
    { showGhostPiece : Bool
    , showVerticalStripes : Bool
    }


initModel : Model
initModel =
    { fallingPiece = Nothing
    , ghostPiece = []
    , bottomBlocks = []
    , board = createBoard game.columns game.rows []
    , dropAnimationTimer = NoInterval
    , lockDelay = zeroLockDelay
    , fullRowsDelayTimer = NoInterval
    , screen = StartDialog { startLevel = initSidePanel.level }
    , sidePanel = initSidePanel
    , settings =
        { showGhostPiece = True
        , showVerticalStripes = False
        }
    }



-- UPDATE


updatePlayScreen : Msg -> Model -> ( Model, Cmd Msg )
updatePlayScreen msg model =
    case msg of
        NewShape ->
            let
                ( sidePanel, cmd ) =
                    updateSidePanelForNewShape model.sidePanel
            in
            ( { model | sidePanel = sidePanel }
            , cmd
            )

        ShapesGenerated shapes ->
            let
                ( sidePanel, cmd ) =
                    updateSidePanelForShapesGenerated shapes model.sidePanel
            in
            ( { model | sidePanel = sidePanel }
            , cmd
            )

        Spawn shape ->
            updateForSpawn shape model

        AnimationFrame timeDelta ->
            updateForAnimationFrame timeDelta model

        AnimationMoveDown rows ->
            updateForAnimationMoveDown rows model

        MoveDown ->
            updateForKeyboardMove
                (translateBy { dCol = 0, dRow = 1 })
                noAlternatives
                True
                model

        MoveLeft ->
            updateForKeyboardMove
                (translateBy { dCol = -1, dRow = 0 })
                noAlternatives
                False
                model

        MoveRight ->
            updateForKeyboardMove
                (translateBy { dCol = 1, dRow = 0 })
                noAlternatives
                False
                model

        RotateClockwise ->
            updateForKeyboardMove
                (Tetromino.rotate Clockwise)
                (wallKickAlternatives Clockwise)
                False
                model

        RotateCounterclockwise ->
            updateForKeyboardMove
                (Tetromino.rotate Counterclockwise)
                (wallKickAlternatives Counterclockwise)
                False
                model

        DropAndLock ->
            updateForDropAndLock model

        LockToBottom ->
            updateForLockToBottom model

        RemoveFullRows ->
            updateForRemoveFullRows model

        ShowQuitDialog ->
            ( { model | screen = QuitDialog { returnScreen = model.screen } }
            , Cmd.none
            )

        TogglePauseDialog ->
            ( { model | screen = PauseDialog { afterCmd = Cmd.none } }
            , Cmd.none
            )

        Exit ->
            ( model
            , triggerMsg TogglePauseDialog
            )

        ToggleHelpDialog ->
            ( { model | screen = HelpDialog { returnScreen = model.screen } }
            , Cmd.none
            )

        ResetGame ->
            updateForResetGame model

        StartGame startLevel ->
            updateForStartGame startLevel model

        Unpause afterCmd ->
            updateForUnpause afterCmd model

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

        WindowMinimized ->
            ( model
            , triggerMsg TogglePauseDialog
            )

        _ ->
            ( model
            , Cmd.none
            )


updateForSpawn : Shape -> Model -> ( Model, Cmd Msg )
updateForSpawn shape model =
    let
        spawnedPiece =
            createTetromino shape
                |> centerHoriz game.columns
                |> translateBy
                    { dCol = 0
                    , dRow = spawningRow model.sidePanel.level
                    }

        ( _, pieceBottomRow ) =
            rowRange spawnedPiece.blocks

        gameOver =
            collision spawnedPiece.blocks model.board
    in
    if not gameOver then
        ( { model
            | fallingPiece = Just spawnedPiece
            , ghostPiece = calculateGhostPiece spawnedPiece.blocks model.board
            , dropAnimationTimer = initialInterval SpawningDropAnimationInterval model.sidePanel.level
            , lockDelay =
                { timer = initialInterval LockDelayInterval model.sidePanel.level
                , movesRemaining = maxLockDelayMoves
                , maxRowReached = pieceBottomRow
                }
            , fullRowsDelayTimer = NoInterval
          }
        , Cmd.none
        )

    else
        ( { model | screen = GameOverDialog }
        , Cmd.none
        )


updateForAnimationFrame : Float -> Model -> ( Model, Cmd Msg )
updateForAnimationFrame timeDelta model =
    let
        lockDelay =
            model.lockDelay

        ( dropAnimationTimer, dropAnimationCmd ) =
            updateTimer
                timeDelta
                (initialInterval DropAnimationInterval model.sidePanel.level)
                (\repeats -> triggerMsg (AnimationMoveDown repeats))
                model.dropAnimationTimer

        ( lockDelayTimer, lockDelayCmd ) =
            updateTimer
                timeDelta
                NoInterval
                (\_ -> triggerMsg LockToBottom)
                lockDelay.timer

        ( fullRowsDelayTimer, fullRowsDelayCmd ) =
            updateTimer
                timeDelta
                NoInterval
                (\_ -> triggerMsg RemoveFullRows)
                model.fullRowsDelayTimer

        ( sidePanel, sidePanelCmd ) =
            updateSidePanelForAnimationFrame timeDelta model.sidePanel
    in
    ( { model
        | dropAnimationTimer = dropAnimationTimer
        , lockDelay = { lockDelay | timer = lockDelayTimer }
        , fullRowsDelayTimer = fullRowsDelayTimer
        , sidePanel = sidePanel
      }
    , Cmd.batch
        [ dropAnimationCmd
        , lockDelayCmd
        , fullRowsDelayCmd
        , sidePanelCmd
        ]
    )


updateForAnimationMoveDown : Int -> Model -> ( Model, Cmd Msg )
updateForAnimationMoveDown rows model =
    case model.fallingPiece of
        Just fallingPiece ->
            let
                distanceToBottom =
                    vertDistance fallingPiece.blocks model.ghostPiece

                dRow =
                    Basics.min rows distanceToBottom
            in
            if dRow > 0 then
                let
                    movedPiece =
                        translateBy { dCol = 0, dRow = dRow } fallingPiece
                in
                updateForActualMove movedPiece False model

            else
                ( model
                , Cmd.none
                )

        Nothing ->
            ( model
            , Cmd.none
            )


updateForKeyboardMove : (Tetromino -> Tetromino) -> (Tetromino -> List Translation) -> Bool -> Model -> ( Model, Cmd Msg )
updateForKeyboardMove move alternativeTranslations softDrop model =
    case model.fallingPiece of
        Just fallingPiece ->
            let
                maybeMovedPiece =
                    firstViableAlternative
                        (alternativeTranslations fallingPiece)
                        (move fallingPiece)
                        model.board
            in
            case maybeMovedPiece of
                Just movedPiece ->
                    updateForActualMove movedPiece softDrop model

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        Nothing ->
            ( model
            , Cmd.none
            )


updateForActualMove : Tetromino -> Bool -> Model -> ( Model, Cmd Msg )
updateForActualMove movedPiece softDrop model =
    let
        ( _, pieceBottomRow ) =
            rowRange movedPiece.blocks

        lockDelay =
            updateLockDelay pieceBottomRow model.sidePanel.level model.lockDelay

        ( sidePanel, sidePanelCmd ) =
            updateSidePanelForMove softDrop model.sidePanel
    in
    ( { model
        | fallingPiece = Just movedPiece
        , ghostPiece = calculateGhostPiece movedPiece.blocks model.board
        , lockDelay = lockDelay
        , sidePanel = sidePanel
      }
    , sidePanelCmd
    )


updateLockDelay : Int -> Int -> LockDelay -> LockDelay
updateLockDelay pieceBottomRow level lockDelay =
    let
        maxRowReached =
            Basics.max pieceBottomRow lockDelay.maxRowReached

        ( movesRemaining, timer ) =
            if maxRowReached > lockDelay.maxRowReached then
                ( maxLockDelayMoves
                , initialInterval LockDelayInterval level
                )

            else if lockDelay.movesRemaining > 0 then
                ( lockDelay.movesRemaining - 1
                , initialInterval LockDelayInterval level
                )

            else if lockDelay.timer /= NoInterval then
                ( 0
                , lockDelay.timer
                )

            else
                ( 0
                  -- schedule LockToBottom on next animation frame
                , Interval 0
                )
    in
    { timer = timer
    , movesRemaining = movesRemaining
    , maxRowReached = maxRowReached
    }


updateForDropAndLock : Model -> ( Model, Cmd Msg )
updateForDropAndLock model =
    case model.fallingPiece of
        Just fallingPiece ->
            let
                droppedPiece =
                    translateVertToTarget fallingPiece model.ghostPiece

                distanceDropped =
                    vertDistance fallingPiece.blocks droppedPiece.blocks

                ( sidePanel, sidePanelCmd ) =
                    updateSidePanelForDropAndLock distanceDropped model.sidePanel
            in
            ( { model
                | fallingPiece = Just droppedPiece
                , ghostPiece = calculateGhostPiece droppedPiece.blocks model.board
                , lockDelay = zeroLockDelay -- schedule LockToBottom on next animation frame
                , sidePanel = sidePanel
              }
            , sidePanelCmd
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
            if hasReachedBottom && model.lockDelay.timer == NoInterval then
                let
                    ( _, pieceBottomRow ) =
                        rowRange fallingPiece.blocks

                    gameOver =
                        pieceBottomRow < 0
                in
                if not gameOver then
                    let
                        bottomBlocks =
                            model.bottomBlocks ++ fallingPiece.blocks

                        hasFullRows =
                            not (List.isEmpty (fullRows game.columns bottomBlocks))

                        ( fullRowsDelayTimer, command ) =
                            if hasFullRows then
                                ( initialInterval FullRowsDelayInterval model.sidePanel.level
                                , Cmd.none
                                )

                            else
                                ( NoInterval
                                , triggerMsg NewShape
                                )
                    in
                    ( { model
                        | fallingPiece = Nothing
                        , ghostPiece = []
                        , bottomBlocks = bottomBlocks
                        , board = createBoard game.columns game.rows bottomBlocks
                        , dropAnimationTimer = NoInterval
                        , fullRowsDelayTimer = fullRowsDelayTimer
                      }
                    , command
                    )

                else
                    ( { model | screen = GameOverDialog }
                    , Cmd.none
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
        ( bottomBlocks, rowsRemoved ) =
            removeFullRows game.columns model.bottomBlocks

        ( sidePanel, sidePanelCmd ) =
            updateSidePanelForRemoveFullRows rowsRemoved model.sidePanel
    in
    ( { model
        | fallingPiece = Nothing
        , ghostPiece = []
        , bottomBlocks = bottomBlocks
        , board = createBoard game.columns game.rows bottomBlocks
        , sidePanel = sidePanel
      }
    , Cmd.batch [ triggerMsg NewShape, sidePanelCmd ]
    )


updateForResetGame : Model -> ( Model, Cmd Msg )
updateForResetGame model =
    ( { initModel | settings = model.settings }
    , Cmd.none
    )


updateForStartGame : Int -> Model -> ( Model, Cmd Msg )
updateForStartGame startLevel model =
    let
        ( sidePanel, sidePanelCmd ) =
            updateSidePanelForStartGame startLevel model.sidePanel
    in
    ( { model | sidePanel = sidePanel }
    , Cmd.batch [ triggerMsg (Unpause (triggerMsg NewShape)), sidePanelCmd ]
    )


updateForUnpause : Cmd Msg -> Model -> ( Model, Cmd Msg )
updateForUnpause afterCmd model =
    ( { model
        | screen =
            CountdownScreen
                { timer = initialInterval CountdownInterval model.sidePanel.level
                , afterCmd = afterCmd
                }
      }
    , Cmd.none
    )


type IntervalType
    = SpawningDropAnimationInterval
    | DropAnimationInterval
    | LockDelayInterval
    | FullRowsDelayInterval
    | CountdownInterval


initialInterval : IntervalType -> Int -> TimeInterval
initialInterval intervalType level =
    case intervalType of
        SpawningDropAnimationInterval ->
            if level == 0 then
                NoInterval

            else
                Interval 0

        DropAnimationInterval ->
            if level == 0 then
                NoInterval

            else
                Dict.get level dropAnimationIntervals
                    |> Maybe.withDefault NoInterval

        LockDelayInterval ->
            Interval 500

        FullRowsDelayInterval ->
            Interval 200

        CountdownInterval ->
            Interval 250



-- See https://tetris.fandom.com/wiki/Tetris_Worlds, section Gravity.
-- Formula: 1000 * pow (0.8 - ((toFloat level - 1) * 0.007)) (level - 1),
-- where (pow x n) is x to the nth power.


dropAnimationIntervals : Dict Int TimeInterval
dropAnimationIntervals =
    Dict.fromList
        [ ( 1, Interval 1000 )
        , ( 2, Interval 793 )
        , ( 3, Interval 617.8 )
        , ( 4, Interval 472.73 )
        , ( 5, Interval 355.2 )
        , ( 6, Interval 262 )
        , ( 7, Interval 189.68 )
        , ( 8, Interval 134.73 )
        , ( 9, Interval 93.88 )
        , ( 10, Interval 64.15 )
        , ( 11, Interval 42.98 )
        , ( 12, Interval 28.22 )
        , ( 13, Interval 18.15 )
        , ( 14, Interval 11.44 )
        , ( 15, Interval 7.06 )
        , ( 16, Interval 4.26 )
        , ( 17, Interval 2.52 )
        , ( 18, Interval 1.46 )
        , ( 19, Interval 0.82 )
        , ( 20, Interval 0.46 )
        ]


spawningRow : Int -> Int
spawningRow level =
    if level == 0 then
        0

    else
        -2


maxLockDelayMoves : Int
maxLockDelayMoves =
    15


zeroLockDelay : LockDelay
zeroLockDelay =
    -- lock immediately (if on the bottom now, or when moving there later)
    { timer = Interval 0
    , movesRemaining = 0
    , maxRowReached = game.rows
    }



-- VIEW


viewPlayScreen : Model -> Svg msg
viewPlayScreen model =
    let
        showPreviews =
            model.screen /= GameOverDialog && model.screen /= HelpDialog { returnScreen = GameOverDialog }
    in
    g
        []
        [ lazy2 viewSidePanel showPreviews model.sidePanel
        , viewBoard
        , lazy viewVerticalStripes model.settings.showVerticalStripes
        , lazy viewBoardBlocks model.bottomBlocks
        , lazy2 viewGhostPiece model.settings.showGhostPiece model.ghostPiece
        , lazy viewFallingPiece model.fallingPiece
        ]


viewBoard : Svg msg
viewBoard =
    rect
        [ x (String.fromFloat -(boardStyle.borderWidth / 2 + boardStyle.padding))
        , y (String.fromFloat -(boardStyle.borderWidth / 2 + boardStyle.padding))
        , width (String.fromFloat (boardWidth - boardStyle.borderWidth))
        , height (String.fromFloat (boardHeight - boardStyle.borderWidth))
        , fill "transparent"
        , stroke "#AAA"
        , strokeWidth (String.fromFloat boardStyle.borderWidth)
        ]
        []


viewBoardBlocks : List Block -> Svg msg
viewBoardBlocks blocks =
    g
        []
        (List.filter
            (\block -> 0 <= block.col && block.col < game.columns && 0 <= block.row && block.row < game.rows)
            blocks
            |> List.map (lazy viewBoardBlock)
        )


viewBoardBlock : Block -> Svg msg
viewBoardBlock block =
    viewBlock ( 0, 0 ) block


viewGhostPiece : Bool -> List Block -> Svg msg
viewGhostPiece visible blocks =
    if visible then
        viewBoardBlocks blocks

    else
        g [] []


viewFallingPiece : Maybe Tetromino -> Svg msg
viewFallingPiece fallingPiece =
    let
        blocks =
            Maybe.map .blocks fallingPiece
                |> Maybe.withDefault []
    in
    viewBoardBlocks blocks


viewVerticalStripes : Bool -> Svg msg
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
