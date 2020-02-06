module Common exposing (..)

import Block exposing (Block, colorToHex)
import Shape exposing (Shape)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task


game : { columns : Int, rows : Int }
game =
    { columns = 10
    , rows = 20
    }



-- UPDATE


type Msg
    = NewShape
    | ShapesGenerated (List Shape)
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
    | ExitStartDialog
    | ShowRestartDialog
    | TogglePauseDialog
    | ToggleHelpDialog
    | AnswerYes
    | AnswerNo
    | Exit
    | NewGame
    | Unpause (Cmd Msg)
    | StopCountdown
    | LevelUp
    | LevelDown
    | WindowMinimized
    | NoOp


type Screen
    = PlayScreen
    | CountdownScreen { timer : TimeInterval, afterCmd : Cmd Msg }
    | StartDialog
    | GameOverDialog
    | RestartDialog
    | PauseDialog { afterCmd : Cmd Msg }
    | HelpDialog { returnScreen : Screen }


type alias TimeInterval =
    Maybe Float


updateTimer : TimeInterval -> Float -> TimeInterval -> Msg -> ( TimeInterval, Cmd Msg )
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


triggerMessage : Msg -> Cmd Msg
triggerMessage msg =
    Task.perform (always msg) (Task.succeed ())


maxLevel : Int
maxLevel =
    12



-- VIEW


boardStyle =
    { margin = 12.0
    , borderWidth = 2.0
    , padding = 1.5
    }


blockStyle =
    { size = 35.0
    , borderWidth = 0.5
    }


boardWidth : Float
boardWidth =
    boardSize game.columns


boardHeight : Float
boardHeight =
    boardSize game.rows


boardSize : Int -> Float
boardSize blockCount =
    toFloat blockCount * blockStyle.size + 2 * (boardStyle.borderWidth + boardStyle.padding)


viewBlock : ( Float, Float ) -> Block -> Svg msg
viewBlock ( originX, originY ) block =
    rect
        [ x (String.fromFloat (originX + toFloat block.col * blockStyle.size + blockStyle.borderWidth / 2))
        , y (String.fromFloat (originY + toFloat block.row * blockStyle.size + blockStyle.borderWidth / 2))
        , width (String.fromFloat (blockStyle.size - blockStyle.borderWidth))
        , height (String.fromFloat (blockStyle.size - blockStyle.borderWidth))
        , fill (colorToHex block.color)
        , stroke "white"
        , strokeWidth (String.fromFloat blockStyle.borderWidth)
        ]
        []
