-- Copyright 2020, Adrian Istrate


module Main exposing (..)

import Browser
import Browser.Events
import Common exposing (..)
import Dialogs exposing (..)
import Json.Decode as Decode
import PlayScreen exposing (..)
import SidePanel exposing (sidePanelStyle)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Lazy exposing (..)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    if model.screen == PlayScreen then
        updatePlayScreen msg model

    else
        let
            ( screen, cmd ) =
                updateDialog msg model.screen
        in
        ( { model | screen = screen }
        , cmd
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown keyDecoder
        , case model.screen of
            PlayScreen ->
                Browser.Events.onAnimationFrameDelta AnimationFrame

            CountdownScreen _ ->
                Browser.Events.onAnimationFrameDelta AnimationFrame

            _ ->
                Sub.none
        , Browser.Events.onVisibilityChange visibilityChanged
        ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKeyboardMsg (Decode.field "key" Decode.string)


toKeyboardMsg : String -> Msg
toKeyboardMsg key =
    case String.toLower key of
        "arrowleft" ->
            MoveLeft

        -- IE
        "left" ->
            MoveLeft

        "arrowright" ->
            MoveRight

        -- IE
        "right" ->
            MoveRight

        "arrowdown" ->
            MoveDown

        -- IE
        "down" ->
            MoveDown

        "arrowup" ->
            RotateClockwise

        -- IE
        "up" ->
            RotateClockwise

        "x" ->
            RotateClockwise

        "control" ->
            RotateCounterclockwise

        "z" ->
            RotateCounterclockwise

        " " ->
            DropAndLock

        -- IE
        "spacebar" ->
            DropAndLock

        "s" ->
            ExitStartDialog

        "q" ->
            ShowQuitDialog

        "p" ->
            TogglePauseDialog

        "h" ->
            ToggleHelpDialog

        "y" ->
            AnswerYes

        "n" ->
            AnswerNo

        "escape" ->
            Exit

        -- IE
        "esc" ->
            Exit

        "+" ->
            LevelUp

        -- IE (numpad)
        "add" ->
            LevelUp

        "-" ->
            LevelDown

        -- IE (numpad)
        "subtract" ->
            LevelDown

        "g" ->
            ToggleGhostPiece

        "v" ->
            ToggleVerticalStripes

        _ ->
            NoOp


visibilityChanged : Browser.Events.Visibility -> Msg
visibilityChanged visibility =
    case visibility of
        Browser.Events.Hidden ->
            WindowMinimized

        Browser.Events.Visible ->
            NoOp



-- VIEW


view : Model -> Svg msg
view model =
    svg
        rootSvgAttributes
        [ lazy viewPlayScreen model
        , lazy viewDialogIfAny model.screen
        ]


rootSvgAttributes : List (Attribute msg)
rootSvgAttributes =
    let
        viewBoxHeight =
            2 * boardStyle.margin + boardHeight
    in
    [ width "100%"
    , height "100%"
    , Svg.Attributes.style
        ("position: fixed; max-height: " ++ String.fromFloat viewBoxHeight ++ "px;")
    , preserveAspectRatio "xMidYMin"
    , viewBox
        (String.fromFloat -(boardStyle.margin + boardStyle.borderWidth + boardStyle.padding)
            ++ " "
            ++ String.fromFloat -(boardStyle.margin + boardStyle.borderWidth + boardStyle.padding)
            ++ " "
            ++ String.fromFloat (2 * boardStyle.margin + boardWidth + sidePanelStyle.width + sidePanelStyle.marginRight)
            ++ " "
            ++ String.fromFloat viewBoxHeight
        )
    , fontFamily "sans-serif"
    , fill "#222"
    ]
