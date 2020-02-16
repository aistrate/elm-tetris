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
    | AnimationMoveDown Int
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
    | ShowQuitDialog
    | TogglePauseDialog
    | ToggleHelpDialog
    | AnswerYes
    | AnswerNo
    | Exit
    | ResetGame
    | StartGame Int
    | Unpause (Cmd Msg)
    | StopCountdown
    | LevelUp
    | LevelDown
    | WindowMinimized
    | NoOp


type alias TimeInterval =
    Maybe Float


updateTimer : TimeInterval -> Float -> TimeInterval -> (Int -> Msg) -> ( TimeInterval, Cmd Msg )
updateTimer timer timeDelta repeatInterval message =
    case timer of
        Just justTimer ->
            let
                newTimer =
                    justTimer - timeDelta
            in
            if newTimer <= 0 then
                case repeatInterval of
                    Just justRepeatInterval ->
                        let
                            repeats =
                                floor (-newTimer / justRepeatInterval) + 1
                        in
                        ( Just (newTimer + justRepeatInterval * toFloat repeats)
                        , triggerMsg (message repeats)
                        )

                    Nothing ->
                        ( Nothing
                        , triggerMsg (message 1)
                        )

            else
                ( Just newTimer
                , Cmd.none
                )

        Nothing ->
            ( Nothing
            , Cmd.none
            )


triggerMsg : Msg -> Cmd Msg
triggerMsg msg =
    Task.perform (always msg) (Task.succeed ())


minLevel : Int
minLevel =
    0


maxLevel : Int
maxLevel =
    15



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
