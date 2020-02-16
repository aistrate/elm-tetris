module Dialogs exposing (..)

import Common exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Lazy exposing (..)
import Timer exposing (..)



-- MODEL


type Screen
    = PlayScreen
    | CountdownScreen { timer : TimeInterval, afterCmd : Cmd Msg }
    | StartDialog { startLevel : Int }
    | GameOverDialog
    | QuitDialog { returnScreen : Screen }
    | PauseDialog { afterCmd : Cmd Msg }
    | HelpDialog { returnScreen : Screen }



-- UPDATE


updateDialog : Msg -> Screen -> ( Screen, Cmd Msg )
updateDialog msg screen =
    case screen of
        CountdownScreen { timer, afterCmd } ->
            updateCountdownScreen timer afterCmd msg screen

        StartDialog { startLevel } ->
            updateStartDialog startLevel msg screen

        GameOverDialog ->
            updateGameOverDialog msg screen

        QuitDialog { returnScreen } ->
            updateQuitDialog returnScreen msg screen

        PauseDialog { afterCmd } ->
            updatePauseDialog afterCmd msg screen

        HelpDialog { returnScreen } ->
            updateHelpDialog returnScreen msg screen

        _ ->
            ( screen, Cmd.none )


updateCountdownScreen : TimeInterval -> Cmd Msg -> Msg -> Screen -> ( Screen, Cmd Msg )
updateCountdownScreen timer afterCmd msg screen =
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
                                (\_ -> triggerMsg StopCountdown)
                    in
                    ( CountdownScreen
                        { timer = updatedTimer
                        , afterCmd = afterCmd
                        }
                    , cmd
                    )

                Nothing ->
                    ( screen
                    , triggerMsg StopCountdown
                    )

        StopCountdown ->
            ( PlayScreen
            , afterCmd
            )

        TogglePauseDialog ->
            ( PauseDialog { afterCmd = afterCmd }
            , Cmd.none
            )

        Exit ->
            ( screen
            , triggerMsg TogglePauseDialog
            )

        WindowMinimized ->
            ( PauseDialog { afterCmd = afterCmd }
            , Cmd.none
            )

        _ ->
            ( screen
            , Cmd.none
            )


updateStartDialog : Int -> Msg -> Screen -> ( Screen, Cmd Msg )
updateStartDialog startLevel msg screen =
    case msg of
        ExitStartDialog ->
            ( screen
            , triggerMsg Exit
            )

        Exit ->
            ( PlayScreen
            , triggerMsg (StartGame startLevel)
            )

        LevelUp ->
            ( StartDialog { startLevel = Basics.min maxLevel (startLevel + 1) }
            , Cmd.none
            )

        LevelDown ->
            ( StartDialog { startLevel = Basics.max minLevel (startLevel - 1) }
            , Cmd.none
            )

        ToggleHelpDialog ->
            ( HelpDialog { returnScreen = screen }
            , Cmd.none
            )

        _ ->
            ( screen
            , Cmd.none
            )


updateGameOverDialog : Msg -> Screen -> ( Screen, Cmd Msg )
updateGameOverDialog msg screen =
    case msg of
        AnswerYes ->
            ( PlayScreen
            , triggerMsg ResetGame
            )

        ToggleHelpDialog ->
            ( HelpDialog { returnScreen = screen }
            , Cmd.none
            )

        _ ->
            ( screen
            , Cmd.none
            )


updateQuitDialog : Screen -> Msg -> Screen -> ( Screen, Cmd Msg )
updateQuitDialog returnScreen msg screen =
    case msg of
        AnswerYes ->
            ( PlayScreen
            , triggerMsg ResetGame
            )

        AnswerNo ->
            ( screen
            , triggerMsg Exit
            )

        Exit ->
            ( returnScreen
            , if returnScreen == PlayScreen then
                triggerMsg (Unpause Cmd.none)

              else
                Cmd.none
            )

        ToggleHelpDialog ->
            ( HelpDialog { returnScreen = screen }
            , Cmd.none
            )

        _ ->
            ( screen
            , Cmd.none
            )


updatePauseDialog : Cmd Msg -> Msg -> Screen -> ( Screen, Cmd Msg )
updatePauseDialog afterCmd msg screen =
    case msg of
        TogglePauseDialog ->
            ( screen
            , triggerMsg Exit
            )

        Exit ->
            ( PlayScreen
            , triggerMsg (Unpause afterCmd)
            )

        ShowQuitDialog ->
            ( QuitDialog { returnScreen = screen }
            , Cmd.none
            )

        ToggleHelpDialog ->
            ( HelpDialog { returnScreen = screen }
            , Cmd.none
            )

        _ ->
            ( screen
            , Cmd.none
            )


updateHelpDialog : Screen -> Msg -> Screen -> ( Screen, Cmd Msg )
updateHelpDialog returnScreen msg screen =
    case msg of
        ToggleHelpDialog ->
            ( screen
            , triggerMsg Exit
            )

        Exit ->
            ( returnScreen
            , if returnScreen == PlayScreen then
                triggerMsg (Unpause Cmd.none)

              else
                Cmd.none
            )

        _ ->
            ( screen
            , Cmd.none
            )



-- VIEW


viewDialogIfAny : Screen -> Svg msg
viewDialogIfAny screen =
    case screen of
        PlayScreen ->
            g [] []

        CountdownScreen { timer } ->
            viewCountdownScreen timer

        StartDialog { startLevel } ->
            viewStartDialog startLevel

        GameOverDialog ->
            viewGameOverDialog

        QuitDialog _ ->
            viewQuitDialog

        PauseDialog _ ->
            viewPauseDialog

        HelpDialog _ ->
            viewHelpDialog


viewCountdownScreen : TimeInterval -> Svg msg
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


viewCountdownText : Int -> Svg msg
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


viewStartDialog : Int -> Svg msg
viewStartDialog startLevel =
    let
        startLevelExplanation =
            if startLevel == 0 then
                "(fixed level; zero gravity)"

            else
                " "
    in
    viewDialog
        [ LargeText "Press Esc or S to start"
        , EmptyLine
        , LargeText "Use +/- to change the start level"
        , LargeText ("Start level: " ++ String.fromInt startLevel)
        , LargeText startLevelExplanation
        ]


viewGameOverDialog : Svg msg
viewGameOverDialog =
    viewDialog
        [ LargeText "Game Over"
        , EmptyLine
        , LargeText "Start new game? (Y/N)"
        ]


viewQuitDialog : Svg msg
viewQuitDialog =
    viewDialog
        [ LargeText "Quit game? (Y/N)"
        ]


viewPauseDialog : Svg msg
viewPauseDialog =
    viewDialog
        [ LargeText "Paused"
        , EmptyLine
        , LargeText "Press Esc or P to resume"
        , EmptyLine
        , LargeText "Press Q to quit game"
        ]


viewHelpDialog : Svg msg
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
        , Shortcut "Q" "Quit game"
        , EmptyLine
        , Shortcut "G" "Ghost piece"
        , Shortcut "V" "Vertical stripes"
        , EmptyLine
        , LargeText "Press Esc or H to exit"
        ]


viewDialog : List DialogTextLine -> Svg msg
viewDialog textLines =
    let
        vertCenteredFirstRow =
            (game.rows - List.length textLines) // 2

        firstLineY =
            String.fromFloat ((toFloat vertCenteredFirstRow + 0.7) * blockStyle.size)

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


viewDialogTextLine : Attribute msg -> DialogTextLine -> Svg msg
viewDialogTextLine yCoord textLine =
    case textLine of
        LargeText largeText ->
            tspan
                [ x (String.fromFloat middleBoardX)
                , yCoord
                , textAnchor "middle"
                , fontSize (String.fromFloat (blockStyle.size * 0.6))
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


viewDialogOverlay : Svg msg
viewDialogOverlay =
    rect
        [ x "0"
        , y "0"
        , width (String.fromFloat (toFloat game.columns * blockStyle.size))
        , height (String.fromFloat (toFloat game.rows * blockStyle.size))
        , fill "white"
        , opacity "0.7"
        ]
        []


middleBoardX : Float
middleBoardX =
    toFloat game.columns * blockStyle.size / 2
