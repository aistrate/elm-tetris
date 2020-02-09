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
    }


initSidePanel : SidePanel
initSidePanel =
    { score = 0
    , level = 1
    , lines = 0
    , time = 0
    , unusedShapes = []
    , previewShapes = []
    }


previewCount : Int
previewCount =
    1



-- UPDATE


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
        , triggerMessage (Spawn fallingPieceShape)
        )

    else
        ( sidePanel
        , Random.generate ShapesGenerated sevenBagShapeGenerator
        )


updateSidePanelForShapesGenerated : List Shape -> SidePanel -> ( SidePanel, Cmd Msg )
updateSidePanelForShapesGenerated shapes sidePanel =
    ( { sidePanel | unusedShapes = sidePanel.unusedShapes ++ shapes }
    , triggerMessage NewShape
    )



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
    viewStatistic 1 "Level" (String.fromInt level)


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
            , fontFamily "Courier New, sans-serif"
            , fontWeight "bold"
            ]
            [ text value
            ]
        ]


viewPreviewShapes : List Shape -> Svg msg
viewPreviewShapes shapes =
    let
        startRow =
            9
    in
    g
        []
        (List.indexedMap
            (\index shape -> viewPreviewShape (startRow + 3 * index) shape)
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
