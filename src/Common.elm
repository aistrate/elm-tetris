module Common exposing (..)

import Shape exposing (..)
import Task


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


type Screen
    = PlayScreen
    | CountdownScreen { timer : Maybe Float, afterCmd : Cmd Msg }
    | GameOverDialog
    | RestartDialog
    | PauseDialog
    | HelpDialog { returnScreen : Screen }


triggerMessage : Msg -> Cmd Msg
triggerMessage msg =
    Task.perform (always msg) (Task.succeed ())


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
