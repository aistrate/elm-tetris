module Shape exposing (..)

import Random


type Shape
    = IShape
    | JShape
    | LShape
    | OShape
    | SShape
    | TShape
    | ZShape


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
