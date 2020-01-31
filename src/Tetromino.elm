module Tetromino exposing (..)

import Block exposing (..)
import Random


type alias Tetromino =
    { blocks : List Block
    , pivot : { col : Float, row : Float }
    , shapeSize : ShapeSize
    , rotationState : RotationState
    }


type ShapeSize
    = Size2By2
    | Size3By2
    | Size4By1


type
    RotationState
    -- See https://tetris.wiki/Super_Rotation_System
    = RotationState0 -- spawn state ("upper" horizontal, flat side down)
    | RotationStateR -- state resulting from a clockwise rotation ("right") from spawn
    | RotationState2 -- state resulting from 2 successive rotations in either direction from spawn
    | RotationStateL -- state resulting from a counter-clockwise ("left") rotation from spawn


type Shape
    = IShape
    | JShape
    | LShape
    | OShape
    | SShape
    | TShape
    | ZShape


type alias Translation =
    { dCol : Int
    , dRow : Int
    }


type RotationDirection
    = Clockwise
    | Counterclockwise


shapeBagGenerator : Random.Generator (List Shape)
shapeBagGenerator =
    shuffle [ IShape, JShape, LShape, OShape, SShape, TShape, ZShape ]


shuffle : List a -> Random.Generator (List a)
shuffle list =
    case list of
        [] ->
            Random.constant []

        head :: tail ->
            Random.uniform head tail
                |> Random.andThen
                    (\item ->
                        let
                            remainingItems =
                                List.filter (\i -> i /= item) list
                        in
                        Random.map2 (::)
                            (Random.constant item)
                            (shuffle remainingItems)
                    )


spawnTetromino : Shape -> Tetromino
spawnTetromino shape =
    case shape of
        IShape ->
            { blocks =
                [ { col = 0, row = 1, color = Cyan }
                , { col = 1, row = 1, color = Cyan }
                , { col = 2, row = 1, color = Cyan }
                , { col = 3, row = 1, color = Cyan }
                ]
            , pivot = { col = 1.5, row = 1.5 }
            , shapeSize = Size4By1
            , rotationState = RotationState0
            }

        JShape ->
            { blocks =
                [ { col = 0, row = 0, color = Blue }
                , { col = 0, row = 1, color = Blue }
                , { col = 1, row = 1, color = Blue }
                , { col = 2, row = 1, color = Blue }
                ]
            , pivot = { col = 1, row = 1 }
            , shapeSize = Size3By2
            , rotationState = RotationState0
            }

        LShape ->
            { blocks =
                [ { col = 2, row = 0, color = Orange }
                , { col = 0, row = 1, color = Orange }
                , { col = 1, row = 1, color = Orange }
                , { col = 2, row = 1, color = Orange }
                ]
            , pivot = { col = 1, row = 1 }
            , shapeSize = Size3By2
            , rotationState = RotationState0
            }

        OShape ->
            { blocks =
                [ { col = 0, row = 0, color = Yellow }
                , { col = 1, row = 0, color = Yellow }
                , { col = 0, row = 1, color = Yellow }
                , { col = 1, row = 1, color = Yellow }
                ]
            , pivot = { col = 0.5, row = 0.5 }
            , shapeSize = Size2By2
            , rotationState = RotationState0
            }

        SShape ->
            { blocks =
                [ { col = 1, row = 0, color = Green }
                , { col = 2, row = 0, color = Green }
                , { col = 0, row = 1, color = Green }
                , { col = 1, row = 1, color = Green }
                ]
            , pivot = { col = 1, row = 1 }
            , shapeSize = Size3By2
            , rotationState = RotationState0
            }

        TShape ->
            { blocks =
                [ { col = 1, row = 0, color = Purple }
                , { col = 0, row = 1, color = Purple }
                , { col = 1, row = 1, color = Purple }
                , { col = 2, row = 1, color = Purple }
                ]
            , pivot = { col = 1, row = 1 }
            , shapeSize = Size3By2
            , rotationState = RotationState0
            }

        ZShape ->
            { blocks =
                [ { col = 0, row = 0, color = Red }
                , { col = 1, row = 0, color = Red }
                , { col = 1, row = 1, color = Red }
                , { col = 2, row = 1, color = Red }
                ]
            , pivot = { col = 1, row = 1 }
            , shapeSize = Size3By2
            , rotationState = RotationState0
            }


centerHoriz : Int -> Tetromino -> Tetromino
centerHoriz columns tetromino =
    let
        ( minCol, maxCol ) =
            columnRange tetromino.blocks

        dCol =
            -minCol + (columns - (maxCol - minCol + 1)) // 2
    in
    translateBy { dCol = dCol, dRow = 0 } tetromino


translateVertToTarget : Tetromino -> List Block -> Tetromino
translateVertToTarget tetromino target =
    translateBy
        { dCol = 0
        , dRow = vertDistance tetromino.blocks target
        }
        tetromino


translateBy : Translation -> Tetromino -> Tetromino
translateBy translation tetromino =
    { tetromino
        | blocks =
            List.map
                (\block ->
                    { block
                        | col = block.col + translation.dCol
                        , row = block.row + translation.dRow
                    }
                )
                tetromino.blocks
        , pivot =
            { col = tetromino.pivot.col + toFloat translation.dCol
            , row = tetromino.pivot.row + toFloat translation.dRow
            }
    }


rotate : RotationDirection -> Tetromino -> Tetromino
rotate direction tetromino =
    let
        sign =
            case direction of
                Clockwise ->
                    1

                Counterclockwise ->
                    -1

        rotateBlock block =
            { block
                | col = round (tetromino.pivot.col - sign * (toFloat block.row - tetromino.pivot.row))
                , row = round (tetromino.pivot.row + sign * (toFloat block.col - tetromino.pivot.col))
            }
    in
    { tetromino
        | blocks = List.map rotateBlock tetromino.blocks
        , rotationState = calculateRotationState tetromino.rotationState direction
    }


calculateRotationState : RotationState -> RotationDirection -> RotationState
calculateRotationState currentRotationState direction =
    case ( currentRotationState, direction ) of
        ( RotationState0, Clockwise ) ->
            RotationStateR

        ( RotationStateR, Clockwise ) ->
            RotationState2

        ( RotationState2, Clockwise ) ->
            RotationStateL

        ( RotationStateL, Clockwise ) ->
            RotationState0

        ( RotationState0, Counterclockwise ) ->
            RotationStateL

        ( RotationStateL, Counterclockwise ) ->
            RotationState2

        ( RotationState2, Counterclockwise ) ->
            RotationStateR

        ( RotationStateR, Counterclockwise ) ->
            RotationState0


noAlternatives : Tetromino -> List Translation
noAlternatives _ =
    [ { dCol = 0, dRow = 0 } ]



-- See https://tetris.wiki/Super_Rotation_System for details on tetromino Wall Kicks after rotation


wallKickAlternatives : RotationDirection -> Tetromino -> List Translation
wallKickAlternatives direction tetromino =
    let
        alternatives =
            case ( tetromino.shapeSize, tetromino.rotationState, direction ) of
                --
                -- Size3By2 (JShape, LShape, SShape, TShape, ZShape)
                ( Size3By2, RotationState0, Clockwise ) ->
                    [ ( 0, 0 ), ( -1, 0 ), ( -1, 1 ), ( 0, -2 ), ( -1, -2 ) ]

                ( Size3By2, RotationStateR, Counterclockwise ) ->
                    [ ( 0, 0 ), ( 1, 0 ), ( 1, -1 ), ( 0, 2 ), ( 1, 2 ) ]

                ( Size3By2, RotationStateR, Clockwise ) ->
                    [ ( 0, 0 ), ( 1, 0 ), ( 1, -1 ), ( 0, 2 ), ( 1, 2 ) ]

                ( Size3By2, RotationState2, Counterclockwise ) ->
                    [ ( 0, 0 ), ( -1, 0 ), ( -1, 1 ), ( 0, -2 ), ( -1, -2 ) ]

                ( Size3By2, RotationState2, Clockwise ) ->
                    [ ( 0, 0 ), ( 1, 0 ), ( 1, 1 ), ( 0, -2 ), ( 1, -2 ) ]

                ( Size3By2, RotationStateL, Counterclockwise ) ->
                    [ ( 0, 0 ), ( -1, 0 ), ( -1, -1 ), ( 0, 2 ), ( -1, 2 ) ]

                ( Size3By2, RotationStateL, Clockwise ) ->
                    [ ( 0, 0 ), ( -1, 0 ), ( -1, -1 ), ( 0, 2 ), ( -1, 2 ) ]

                ( Size3By2, RotationState0, Counterclockwise ) ->
                    [ ( 0, 0 ), ( 1, 0 ), ( 1, 1 ), ( 0, -2 ), ( 1, -2 ) ]

                --
                -- Size4By1 (IShape)
                ( Size4By1, RotationState0, Clockwise ) ->
                    [ ( 0, 0 ), ( -2, 0 ), ( 1, 0 ), ( -2, -1 ), ( 1, 2 ) ]

                ( Size4By1, RotationStateR, Counterclockwise ) ->
                    [ ( 0, 0 ), ( 2, 0 ), ( -1, 0 ), ( 2, 1 ), ( -1, -2 ) ]

                ( Size4By1, RotationStateR, Clockwise ) ->
                    [ ( 0, 0 ), ( -1, 0 ), ( 2, 0 ), ( -1, 2 ), ( 2, -1 ) ]

                ( Size4By1, RotationState2, Counterclockwise ) ->
                    [ ( 0, 0 ), ( 1, 0 ), ( -2, 0 ), ( 1, -2 ), ( -2, 1 ) ]

                ( Size4By1, RotationState2, Clockwise ) ->
                    [ ( 0, 0 ), ( 2, 0 ), ( -1, 0 ), ( 2, 1 ), ( -1, -2 ) ]

                ( Size4By1, RotationStateL, Counterclockwise ) ->
                    [ ( 0, 0 ), ( -2, 0 ), ( 1, 0 ), ( -2, -1 ), ( 1, 2 ) ]

                ( Size4By1, RotationStateL, Clockwise ) ->
                    [ ( 0, 0 ), ( 1, 0 ), ( -2, 0 ), ( 1, -2 ), ( -2, 1 ) ]

                ( Size4By1, RotationState0, Counterclockwise ) ->
                    [ ( 0, 0 ), ( -1, 0 ), ( 2, 0 ), ( -1, 2 ), ( 2, -1 ) ]

                --
                -- Size2By2 (OShape)
                ( Size2By2, _, _ ) ->
                    [ ( 0, 0 ) ]
    in
    List.map (\( dCol, dRow ) -> { dCol = dCol, dRow = -dRow }) alternatives
