module Dialogs exposing (..)

import Common exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Lazy exposing (lazy)



-- UPDATE


updateDialog : Msg -> Screen -> ( Screen, Cmd Msg )
updateDialog msg screen =
    case screen of
        CountdownScreen { timer, afterCmd } ->
            updateCountdownScreen timer afterCmd msg screen

        GameOverDialog ->
            updateGameOverDialog msg screen

        RestartDialog ->
            updateRestartDialog msg screen

        PauseDialog ->
            updatePauseDialog msg screen

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
                                StopCountdown
                    in
                    ( CountdownScreen
                        { timer = updatedTimer
                        , afterCmd = afterCmd
                        }
                    , cmd
                    )

                Nothing ->
                    ( screen
                    , triggerMessage StopCountdown
                    )

        StopCountdown ->
            ( PlayScreen
            , afterCmd
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
            , triggerMessage Restart
            )

        ToggleHelpDialog ->
            ( HelpDialog { returnScreen = screen }
            , Cmd.none
            )

        _ ->
            ( screen
            , Cmd.none
            )


updateRestartDialog : Msg -> Screen -> ( Screen, Cmd Msg )
updateRestartDialog msg screen =
    case msg of
        AnswerYes ->
            ( PlayScreen
            , triggerMessage Restart
            )

        AnswerNo ->
            ( screen
            , triggerMessage Exit
            )

        Exit ->
            ( PlayScreen
            , triggerMessage Unpause
            )

        ToggleHelpDialog ->
            ( HelpDialog { returnScreen = screen }
            , Cmd.none
            )

        _ ->
            ( screen
            , Cmd.none
            )


updatePauseDialog : Msg -> Screen -> ( Screen, Cmd Msg )
updatePauseDialog msg screen =
    case msg of
        TogglePauseDialog ->
            ( screen
            , triggerMessage Exit
            )

        Exit ->
            ( PlayScreen
            , triggerMessage Unpause
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
            , triggerMessage Exit
            )

        Exit ->
            ( returnScreen
            , if returnScreen == PlayScreen then
                triggerMessage Unpause

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

        GameOverDialog ->
            viewGameOverDialog

        RestartDialog ->
            viewRestartDialog

        PauseDialog ->
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


viewGameOverDialog : Svg msg
viewGameOverDialog =
    viewDialog
        [ LargeText "Game Over"
        , EmptyLine
        , LargeText "Restart? (Y/N)"
        ]


viewRestartDialog : Svg msg
viewRestartDialog =
    viewDialog
        [ LargeText "Restart? (Y/N)"
        ]


viewPauseDialog : Svg msg
viewPauseDialog =
    viewDialog
        [ LargeText "Paused"
        , EmptyLine
        , LargeText "Press Esc or P to continue"
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


viewDialog : List DialogTextLine -> Svg msg
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


viewDialogTextLine : Attribute msg -> DialogTextLine -> Svg msg
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


viewDialogOverlay : Svg msg
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


middleBoardX : Float
middleBoardX =
    toFloat game.columns * blockStyle.size / 2
