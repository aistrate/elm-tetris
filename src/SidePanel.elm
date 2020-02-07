module SidePanel exposing (..)

import Common exposing (..)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Shape exposing (Shape)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Lazy exposing (..)
import Tetromino exposing (ShapeSize(..), Tetromino, createTetromino)



-- MODEL


type alias SidePanel =
    { level : Int
    , lines : Int
    , time : Float
    , previewShapes : List Shape
    }


previewCount : Int
previewCount =
    1



-- VIEW


sidePanelStyle =
    { x = boardWidth - (boardStyle.borderWidth + boardStyle.padding) + boardStyle.margin
    , y = 0
    , width = blockStyle.size * 6.5
    , height = blockStyle.size * toFloat game.rows
    , marginRight = 12.0
    , paddingLeft = blockStyle.size * 0.5
    , paddingRight = blockStyle.size * 0.1
    , paddingTop = blockStyle.size * 0.1
    , paddingBottom = 3.5
    }


viewSidePanel : SidePanel -> Screen -> Svg msg
viewSidePanel sidePanel screen =
    let
        showPreviews =
            screen /= GameOverDialog

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
            [ lazy viewLevel sidePanel.level
            , lazy viewLines sidePanel.lines
            , lazy viewTime timeInSeconds
            , viewFooter
            ]
        ]


viewLevel : Int -> Svg msg
viewLevel level =
    viewStatistic 0 "Level" (String.fromInt level)


viewLines : Int -> Svg msg
viewLines lines =
    viewStatistic 1 "Lines" (prettyFormatInt lines)


viewTime : Int -> Svg msg
viewTime timeInSeconds =
    viewStatistic 2 "Time" (prettyFormatTime timeInSeconds)


viewStatistic : Int -> String -> String -> Svg msg
viewStatistic row label value =
    tspan
        [ y (String.fromFloat (sidePanelStyle.y + sidePanelStyle.paddingTop + (blockStyle.size * (toFloat row + 0.65))))
        , fontSize (String.fromFloat (blockStyle.size * 0.65))
        ]
        [ tspan
            [ x (String.fromFloat (sidePanelStyle.x + sidePanelStyle.paddingLeft))
            ]
            [ text label
            ]
        , tspan
            [ x (String.fromFloat (sidePanelStyle.x + sidePanelStyle.width - sidePanelStyle.paddingRight))
            , textAnchor "end"
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
            8
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


prettyFormatInt : Int -> String
prettyFormatInt i =
    FormatNumber.format { usLocale | decimals = 0 } (toFloat i)


prettyFormatTime : Int -> String
prettyFormatTime timeInSeconds =
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
