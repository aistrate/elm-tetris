module SidePanel exposing (..)

import Common exposing (..)
import Random
import Shape exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Lazy exposing (..)
import Tetromino exposing (ShapeSize(..), Tetromino, createTetromino)



-- MODEL


type alias SidePanel =
    { score : Int
    , level : Int
    , lines : Int
    , time : Float
    , unusedShapes : List Shape
    , previewShapes : List Shape
    , difficultLineClear : Bool
    , futureBackToBackBonus : Int
    , levelGoal : Int
    }


initSidePanel : SidePanel
initSidePanel =
    { score = 0
    , level = 1
    , lines = 0
    , time = 0
    , unusedShapes = []
    , previewShapes = []
    , difficultLineClear = False
    , futureBackToBackBonus = 0
    , levelGoal = levelGoalStep
    }



-- UPDATE


previewCount : Int
previewCount =
    1


previewStartRow : Int
previewStartRow =
    9


updateSidePanelForNewShape : SidePanel -> ( SidePanel, Cmd Msg )
updateSidePanelForNewShape sidePanel =
    let
        targetCount =
            previewCount + 1

        actualCount =
            List.length sidePanel.previewShapes + List.length sidePanel.unusedShapes
    in
    if actualCount >= targetCount then
        let
            missingCount =
                targetCount - List.length sidePanel.previewShapes

            previewShapes =
                sidePanel.previewShapes ++ List.take missingCount sidePanel.unusedShapes

            fallingPieceShape =
                List.head previewShapes |> Maybe.withDefault IShape
        in
        ( { sidePanel
            | unusedShapes = List.drop missingCount sidePanel.unusedShapes
            , previewShapes = List.drop 1 previewShapes
          }
        , triggerMsg (Spawn fallingPieceShape)
        )

    else
        ( sidePanel
        , Random.generate ShapesGenerated sevenBagShapeGenerator
        )


updateSidePanelForShapesGenerated : List Shape -> SidePanel -> ( SidePanel, Cmd Msg )
updateSidePanelForShapesGenerated shapes sidePanel =
    ( { sidePanel | unusedShapes = sidePanel.unusedShapes ++ shapes }
    , triggerMsg NewShape
    )


updateSidePanelForAnimationFrame : Float -> SidePanel -> ( SidePanel, Cmd Msg )
updateSidePanelForAnimationFrame timeDelta sidePanel =
    ( { sidePanel | time = sidePanel.time + timeDelta }
    , Cmd.none
    )


updateSidePanelForMove : Bool -> SidePanel -> ( SidePanel, Cmd Msg )
updateSidePanelForMove softDrop sidePanel =
    let
        scorePoints =
            if softDrop then
                1

            else
                0
    in
    ( { sidePanel | score = sidePanel.score + scorePoints }
    , Cmd.none
    )


updateSidePanelForDropAndLock : Int -> SidePanel -> ( SidePanel, Cmd Msg )
updateSidePanelForDropAndLock distanceDropped sidePanel =
    ( { sidePanel | score = sidePanel.score + 2 * distanceDropped }
    , Cmd.none
    )


updateSidePanelForRemoveFullRows : Int -> SidePanel -> ( SidePanel, Cmd Msg )
updateSidePanelForRemoveFullRows rowsRemoved sidePanel =
    let
        points =
            calculateScorePoints rowsRemoved sidePanel.level

        difficultLineClear =
            rowsRemoved == 4

        backToBackBonus =
            if sidePanel.difficultLineClear && difficultLineClear then
                sidePanel.futureBackToBackBonus + points // 2

            else
                0

        futureBackToBackBonus =
            if not sidePanel.difficultLineClear && difficultLineClear then
                points // 2

            else
                0

        lines =
            sidePanel.lines + rowsRemoved

        ( level, levelGoal ) =
            if 0 < sidePanel.level && sidePanel.level < maxLevel && lines >= sidePanel.levelGoal then
                ( sidePanel.level + 1
                , sidePanel.levelGoal + levelGoalStep
                )

            else
                ( sidePanel.level
                , sidePanel.levelGoal
                )
    in
    ( { sidePanel
        | score = sidePanel.score + points + backToBackBonus
        , difficultLineClear = difficultLineClear
        , futureBackToBackBonus = futureBackToBackBonus
        , lines = lines
        , level = level
        , levelGoal = levelGoal
      }
    , Cmd.none
    )


updateSidePanelForStartGame : Int -> SidePanel -> ( SidePanel, Cmd Msg )
updateSidePanelForStartGame startLevel sidePanel =
    ( { sidePanel | level = startLevel }
    , Cmd.none
    )


calculateScorePoints : Int -> Int -> Int
calculateScorePoints rowsRemoved level =
    let
        points =
            case rowsRemoved of
                1 ->
                    100

                2 ->
                    300

                3 ->
                    500

                4 ->
                    800

                _ ->
                    0

        adjustedLevel =
            Basics.max 1 level
    in
    points * adjustedLevel


levelGoalStep : Int
levelGoalStep =
    10



-- VIEW


sidePanelStyle =
    { x = boardWidth - (boardStyle.borderWidth + boardStyle.padding) + boardStyle.margin
    , y = 0
    , width = blockStyle.size * 6.5
    , height = blockStyle.size * toFloat game.rows
    , marginRight = 12.0
    , paddingLeft = blockStyle.size * 0.5
    , paddingRight = blockStyle.size * 0.1
    , paddingTop = 0
    , paddingBottom = 3.5
    }


viewSidePanel : Bool -> SidePanel -> Svg msg
viewSidePanel showPreviews sidePanel =
    let
        timeInSeconds =
            floor (sidePanel.time / 1000)
    in
    g
        []
        [ if showPreviews then
            lazy viewPreviewShapes sidePanel.previewShapes

          else
            g [] []
        , text_
            []
            [ lazy viewScore sidePanel.score
            , lazy viewLevel sidePanel.level
            , lazy viewLines sidePanel.lines
            , lazy viewTime timeInSeconds
            , viewFooter
            ]
        ]


viewScore : Int -> Svg msg
viewScore score =
    viewStatistic 0 "Score" (formatInt score)


viewLevel : Int -> Svg msg
viewLevel level =
    let
        levelLabel =
            if level == 0 then
                "Level (fixed)"

            else
                "Level"
    in
    viewStatistic 1 levelLabel (String.fromInt level)


viewLines : Int -> Svg msg
viewLines lines =
    viewStatistic 2 "Lines" (formatInt lines)


viewTime : Int -> Svg msg
viewTime timeInSeconds =
    viewStatistic 3 "Time" (formatTime timeInSeconds)


viewStatistic : Int -> String -> String -> Svg msg
viewStatistic row label value =
    tspan
        [ y (String.fromFloat (sidePanelStyle.y + sidePanelStyle.paddingTop + (blockStyle.size * (toFloat row + 0.7))))
        ]
        [ tspan
            [ x (String.fromFloat (sidePanelStyle.x + sidePanelStyle.paddingLeft))
            , fontSize (String.fromFloat (blockStyle.size * 0.6))
            ]
            [ text label
            ]
        , tspan
            [ x (String.fromFloat (sidePanelStyle.x + sidePanelStyle.width - sidePanelStyle.paddingRight))
            , textAnchor "end"
            , fontSize (String.fromFloat (blockStyle.size * 0.65))
            , fontFamily "Courier New, monospace"
            , fontWeight "bold"
            ]
            [ text value
            ]
        ]


viewPreviewShapes : List Shape -> Svg msg
viewPreviewShapes shapes =
    g
        []
        (List.indexedMap
            (\index shape -> viewPreviewShape (previewStartRow + 3 * index) shape)
            shapes
        )


viewPreviewShape : Int -> Shape -> Svg msg
viewPreviewShape row shape =
    let
        tetromino =
            createTetromino shape

        ( tetrominoCols, tetrominoRows ) =
            tetrominoSize tetromino.shapeSize

        shiftY =
            if tetrominoRows == 1 then
                -0.5 * blockStyle.size

            else
                0

        s =
            sidePanelStyle

        tetrominoX =
            s.x + s.paddingLeft + (s.width - s.paddingLeft - s.paddingRight - toFloat tetrominoCols * blockStyle.size) / 2

        tetrominoY =
            s.y + toFloat row * blockStyle.size + shiftY
    in
    g
        []
        (List.map
            (viewBlock ( tetrominoX, tetrominoY ))
            tetromino.blocks
        )


viewFooter : Svg msg
viewFooter =
    tspan
        [ y (String.fromFloat (sidePanelStyle.y + sidePanelStyle.height - sidePanelStyle.paddingBottom))
        , fontSize "15"
        ]
        [ tspan
            [ x (String.fromFloat (sidePanelStyle.x + sidePanelStyle.paddingLeft))
            ]
            [ text "Press H for Help"
            ]
        , a
            [ xlinkHref "https://github.com/aistrate/elm-tetris"
            , target "_blank"
            , Svg.Attributes.style "fill: #0366D6; text-decoration: underline;"
            ]
            [ tspan
                [ x (String.fromFloat (sidePanelStyle.x + sidePanelStyle.width - sidePanelStyle.paddingRight))
                , textAnchor "end"
                ]
                [ text "Code"
                ]
            ]
        ]


formatInt : Int -> String
formatInt number =
    let
        sign =
            if number < 0 then
                "-"

            else
                ""
    in
    String.fromInt (abs number)
        |> String.reverse
        |> String.toList
        |> splitIntoGroupsOf 3
        |> List.map String.fromList
        |> String.join ","
        |> String.reverse
        |> (++) sign


splitIntoGroupsOf : Int -> List a -> List (List a)
splitIntoGroupsOf n list =
    case list of
        [] ->
            []

        _ ->
            List.take n list :: splitIntoGroupsOf n (List.drop n list)


formatTime : Int -> String
formatTime timeInSeconds =
    let
        seconds =
            modBy 60 timeInSeconds

        minutes =
            modBy 60 (timeInSeconds // 60)

        hours =
            modBy 24 (timeInSeconds // 3600)

        days =
            timeInSeconds // 86400

        pad t =
            String.padLeft 2 '0' (String.fromInt t)

        minSecPadded =
            pad minutes ++ ":" ++ pad seconds

        hrMinSecPadded =
            pad hours ++ ":" ++ minSecPadded
    in
    if days > 0 then
        String.fromInt days ++ "d " ++ hrMinSecPadded

    else if hours > 0 then
        hrMinSecPadded

    else
        minSecPadded


tetrominoSize : ShapeSize -> ( Int, Int )
tetrominoSize shapeSize =
    case shapeSize of
        Size2By2 ->
            ( 2, 2 )

        Size3By2 ->
            ( 3, 2 )

        Size4By1 ->
            ( 4, 1 )
