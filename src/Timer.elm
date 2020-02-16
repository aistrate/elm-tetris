module Timer exposing (..)


type alias TimeInterval =
    Maybe Float


updateTimer : TimeInterval -> Float -> TimeInterval -> (Int -> Cmd msg) -> ( TimeInterval, Cmd msg )
updateTimer timer timeDelta repeatInterval command =
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
                        , command repeats
                        )

                    Nothing ->
                        ( Nothing
                        , command 1
                        )

            else
                ( Just newTimer
                , Cmd.none
                )

        Nothing ->
            ( Nothing
            , Cmd.none
            )
