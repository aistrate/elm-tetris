module Dialogs exposing (..)

import Common exposing (..)



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


updateCountdownScreen : Maybe Float -> Cmd Msg -> Msg -> Screen -> ( Screen, Cmd Msg )
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
