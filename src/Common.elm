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


triggerMsg : msg -> Cmd msg
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
