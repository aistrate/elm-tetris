module SidePanel exposing (..)

import Common exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- MODEL


type alias SidePanel =
    { level : Int
    }



-- VIEW


sidePanelStyle =
    { x = boardWidth - (boardStyle.borderWidth + boardStyle.padding) + boardStyle.margin
    , y = 0
    , width = blockStyle.size * 6.5
    , height = blockStyle.size * toFloat game.rows
    , marginRight = 12.0
    , paddingLeft = blockStyle.size * 1
    , paddingRight = blockStyle.size * 0.1
    , paddingTop = blockStyle.size * 0.1
    , paddingBottom = 3.5
    }


viewSidePanel : SidePanel -> Svg msg
viewSidePanel sidePanel =
    g
        []
        [ text_
            []
            [ tspan
                [ y (String.fromFloat (sidePanelStyle.y + sidePanelStyle.paddingTop + (blockStyle.size * 0.65)))
                , fontSize (String.fromFloat (blockStyle.size * 0.65))
                ]
                [ tspan
                    [ x (String.fromFloat (sidePanelStyle.x + sidePanelStyle.paddingLeft))
                    ]
                    [ text "Level"
                    ]
                , tspan
                    [ x (String.fromFloat (sidePanelStyle.x + sidePanelStyle.width - sidePanelStyle.paddingRight))
                    , textAnchor "end"
                    ]
                    [ text (String.fromInt sidePanel.level)
                    ]
                ]
            , tspan
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
            ]
        ]
