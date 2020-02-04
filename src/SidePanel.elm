module SidePanel exposing (..)

import Common exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- MODEL


type alias SidePanel =
    { level : Int
    }



-- VIEW


viewFooter : SidePanel -> Svg msg
viewFooter sidePanel =
    let
        footerY =
            boardHeight - (boardStyle.borderWidth + boardStyle.padding)

        levelText =
            "Level "
                ++ String.fromInt sidePanel.level
                ++ (if sidePanel.level == 0 then
                        " (no gravity)"

                    else
                        ""
                   )
    in
    g
        []
        [ text_
            []
            [ tspan
                [ x "0"
                , y (String.fromFloat (footerY + blockStyle.size))
                , fontSize (String.fromFloat (blockStyle.size * 0.65))
                ]
                [ text levelText
                ]
            , tspan
                [ y (String.fromFloat (footerY + boardStyle.footerHeight - 4))
                , fontSize "15"
                ]
                [ tspan
                    [ x "0"
                    ]
                    [ text "Press H for Help"
                    ]
                , a
                    [ xlinkHref "https://github.com/aistrate/elm-tetris"
                    , target "_blank"
                    , Svg.Attributes.style "fill: #0366D6; text-decoration: underline;"
                    ]
                    [ tspan
                        [ x (String.fromFloat (blockStyle.size * toFloat game.columns))
                        , textAnchor "end"
                        ]
                        [ text "Code"
                        ]
                    ]
                ]
            ]
        ]
