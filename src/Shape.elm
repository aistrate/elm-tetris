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


sevenBagShapeGenerator : Random.Generator (List Shape)
sevenBagShapeGenerator =
    shuffle [ IShape, JShape, LShape, OShape, SShape, TShape, ZShape ]


singleShapeGenerator : Random.Generator (List Shape)
singleShapeGenerator =
    Random.uniform IShape [ JShape, LShape, OShape, SShape, TShape, ZShape ]
        |> Random.map List.singleton


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
