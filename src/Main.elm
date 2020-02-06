-- Copyright 2020, Adrian Istrate


module Main exposing (..)

import Block exposing (..)
import Board exposing (..)
import Browser
import Browser.Events
import Common exposing (..)
import Dialogs exposing (..)
import Dict exposing (Dict)
import Json.Decode as Decode
import Random
import Shape exposing (..)
import SidePanel exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Lazy exposing (..)
import Tetromino exposing (..)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { unusedShapes : List Shape
    , fallingPiece : Maybe Tetromino
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


init : () -> ( Model, Cmd Msg )
init _ =
    ( { initModel | screen = StartDialog }
    , Cmd.none
    )


initModel : Model
initModel =
    { unusedShapes = []
    , fallingPiece = Nothing
    , ghostPiece = []
    , bottomBlocks = []
    , board = createBoard game.columns game.rows []
    , dropAnimationTimer = Nothing
    , lockDelay = zeroLockDelay
    , fullRowsDelayTimer = Nothing
    , screen = PlayScreen
    , sidePanel =
        { level = 1
        , lines = 0
        , time = 0
        , previewShapes = []
        }
    , settings =
        { showGhostPiece = False
        , showVerticalStripes = False
        }
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LevelUp ->
            updateForLevelChange (model.sidePanel.level + 1) model

        LevelDown ->
            updateForLevelChange (model.sidePanel.level - 1) model

        _ ->
            if model.screen == PlayScreen then
                updatePlayScreen msg model

            else
                let
                    ( screen, cmd ) =
                        updateDialog msg model.screen
                in
                ( { model | screen = screen }
                , cmd
                )


updateForLevelChange : Int -> Model -> ( Model, Cmd Msg )
updateForLevelChange level model =
    let
        sidePanel =
            model.sidePanel

        dropAnimationTimer =
            if
                sidePanel.level
                    == 0
                    && level
                    == 1
                    && model.dropAnimationTimer
                    == Nothing
                    && model.fallingPiece
                    /= Nothing
            then
                initialInterval DropAnimationInterval level

            else
                model.dropAnimationTimer
    in
    ( { model
        | sidePanel = { sidePanel | level = clamp 0 maxLevel level }
        , dropAnimationTimer = dropAnimationTimer
      }
    , Cmd.none
    )


updatePlayScreen : Msg -> Model -> ( Model, Cmd Msg )
updatePlayScreen msg model =
    case msg of
        NewShape ->
            updateForNewShape model

        ShapesGenerated shapes ->
            ( { model | unusedShapes = model.unusedShapes ++ shapes }
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
            ( { model | screen = PauseDialog { afterCmd = Cmd.none } }
            , Cmd.none
            )

        Exit ->
            ( model
            , triggerMessage TogglePauseDialog
            )

        ToggleHelpDialog ->
            ( { model | screen = HelpDialog { returnScreen = model.screen } }
            , Cmd.none
            )

        NewGame ->
            updateForNewGame model

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
            , triggerMessage TogglePauseDialog
            )

        _ ->
            ( model
            , Cmd.none
            )


updateForNewShape : Model -> ( Model, Cmd Msg )
updateForNewShape model =
    case model.unusedShapes of
        shape :: remainingShapes ->
            ( { model | unusedShapes = remainingShapes }
            , triggerMessage (Spawn shape)
            )

        [] ->
            ( model
            , Random.generate ShapesGenerated sevenBagShapeGenerator
            )


updateForSpawn : Shape -> Model -> ( Model, Cmd Msg )
updateForSpawn shape model =
    let
        spawnedPiece =
            spawnTetromino shape
                |> centerHoriz game.columns
                |> translateBy
                    { dCol = 0
                    , dRow = spawningRow model.sidePanel.level
                    }

        ( _, pieceBottomRow ) =
            rowRange spawnedPiece.blocks

        gameOver =
            collision spawnedPiece.blocks model.board

        ( fallingPiece, ghostPiece, screen ) =
            if not gameOver then
                ( Just spawnedPiece
                , calculateGhostPiece spawnedPiece.blocks model.board
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
        , dropAnimationTimer = initialInterval SpawningDropAnimationInterval model.sidePanel.level
        , lockDelay =
            { timer = initialInterval LockDelayInterval model.sidePanel.level
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
                (initialInterval DropAnimationInterval model.sidePanel.level)
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

        sidePanel =
            model.sidePanel
    in
    ( { model
        | dropAnimationTimer = dropAnimationTimer
        , lockDelay = { lockDelay | timer = lockDelayTimer }
        , fullRowsDelayTimer = fullRowsDelayTimer
        , sidePanel = { sidePanel | time = sidePanel.time + timeDelta }
      }
    , Cmd.batch
        [ dropAnimationCmd
        , lockDelayCmd
        , fullRowsDelayCmd
        ]
    )


updateForMove : (Tetromino -> Tetromino) -> (Tetromino -> List Translation) -> Model -> ( Model, Cmd Msg )
updateForMove move alternativeTranslations model =
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
                    let
                        ( _, pieceBottomRow ) =
                            rowRange movedPiece.blocks

                        lockDelay =
                            updateLockDelay pieceBottomRow model.sidePanel.level model.lockDelay
                    in
                    ( { model
                        | fallingPiece = Just movedPiece
                        , ghostPiece = calculateGhostPiece movedPiece.blocks model.board
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
                , ghostPiece = calculateGhostPiece droppedPiece.blocks model.board
                , lockDelay = zeroLockDelay
              }
            , Cmd.none
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
            if hasReachedBottom && model.lockDelay.timer == Nothing then
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
                            ( Nothing
                            , triggerMessage NewShape
                            )
                in
                ( { model
                    | fallingPiece = Nothing
                    , ghostPiece = []
                    , bottomBlocks = bottomBlocks
                    , board = createBoard game.columns game.rows bottomBlocks
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
        ( bottomBlocks, rowsRemoved ) =
            removeFullRows game.columns model.bottomBlocks

        sidePanel =
            model.sidePanel
    in
    ( { model
        | fallingPiece = Nothing
        , ghostPiece = []
        , bottomBlocks = bottomBlocks
        , board = createBoard game.columns game.rows bottomBlocks
        , sidePanel = { sidePanel | lines = sidePanel.lines + rowsRemoved }
      }
    , triggerMessage NewShape
    )


updateForNewGame : Model -> ( Model, Cmd Msg )
updateForNewGame model =
    let
        initSidePanel =
            initModel.sidePanel
    in
    ( { initModel
        | sidePanel = { initSidePanel | level = model.sidePanel.level }
        , settings = model.settings
      }
    , triggerMessage (Unpause (triggerMessage NewShape))
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
                Nothing

            else
                Just 0

        DropAnimationInterval ->
            if level == 0 then
                Nothing

            else
                Dict.get level dropAnimationIntervals
                    |> Maybe.withDefault Nothing

        LockDelayInterval ->
            Just 500

        FullRowsDelayInterval ->
            Just 200

        CountdownInterval ->
            Just 250



-- See https://tetris.fandom.com/wiki/Tetris_Worlds, under heading Gravity.
-- Formula: round (1000 * pow (0.8 - ((toFloat level - 1) * 0.007)) (level - 1)),
-- where (pow x n) is x to the nth power


dropAnimationIntervals : Dict Int TimeInterval
dropAnimationIntervals =
    Dict.fromList
        [ ( 1, Just 1000 )
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
    { timer = Just 0
    , movesRemaining = 0
    , maxRowReached = game.rows
    }



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
        , Browser.Events.onVisibilityChange visibilityChanged
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

        "s" ->
            ExitStartDialog

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

        -- IE (numpad)
        "add" ->
            LevelUp

        "-" ->
            LevelDown

        -- IE (numpad)
        "subtract" ->
            LevelDown

        "g" ->
            ToggleGhostPiece

        "v" ->
            ToggleVerticalStripes

        _ ->
            NoOp


visibilityChanged : Browser.Events.Visibility -> Msg
visibilityChanged visibility =
    case visibility of
        Browser.Events.Hidden ->
            WindowMinimized

        Browser.Events.Visible ->
            NoOp



-- VIEW


view : Model -> Svg msg
view model =
    svg
        rootSvgAttributes
        [ lazy viewSidePanel model.sidePanel
        , viewBoard
        , lazy viewVerticalStripes model.settings.showVerticalStripes
        , lazy viewBoardBlocks model.bottomBlocks
        , lazy2 viewGhostPiece model.settings.showGhostPiece model.ghostPiece
        , lazy viewFallingPiece model.fallingPiece
        , lazy viewDialogIfAny model.screen
        ]


rootSvgAttributes : List (Attribute msg)
rootSvgAttributes =
    [ width "100%"
    , height (String.fromFloat (2 * boardStyle.margin + boardHeight))
    , viewBox
        (String.fromFloat -(boardStyle.margin + boardStyle.borderWidth + boardStyle.padding)
            ++ " "
            ++ String.fromFloat -(boardStyle.margin + boardStyle.borderWidth + boardStyle.padding)
            ++ " "
            ++ String.fromFloat (2 * boardStyle.margin + boardWidth + sidePanelStyle.width + sidePanelStyle.marginRight)
            ++ " "
            ++ String.fromFloat (2 * boardStyle.margin + boardHeight)
        )
    , fontFamily "sans-serif"
    , fill "#222"
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
