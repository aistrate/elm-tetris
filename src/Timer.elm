module Timer exposing (..)


type TimeInterval
    = Interval Float
    | NoInterval


updateTimer : Float -> TimeInterval -> (Int -> Cmd msg) -> TimeInterval -> ( TimeInterval, Cmd msg )
updateTimer timeDelta repeatInterval command timer =
    case timer of
        Interval timerValue ->
            let
                updatedTimerValue =
                    timerValue - timeDelta
            in
            if updatedTimerValue <= 0 then
                case repeatInterval of
                    Interval repeatIntervalValue ->
                        let
                            repeats =
                                floor (-updatedTimerValue / repeatIntervalValue) + 1
                        in
                        ( Interval (updatedTimerValue + repeatIntervalValue * toFloat repeats)
                        , command repeats
                        )

                    NoInterval ->
                        ( NoInterval
                        , command 1
                        )

            else
                ( Interval updatedTimerValue
                , Cmd.none
                )

        NoInterval ->
            ( NoInterval
            , Cmd.none
            )
