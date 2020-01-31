module Block exposing (..)


type alias Block =
    { col : Int
    , row : Int
    , color : Color
    }


type Color
    = Red
    | Green
    | Blue
    | Cyan
    | Orange
    | Purple
    | Yellow
    | Gray


vertDistance : List Block -> List Block -> Int
vertDistance source dest =
    let
        ( sourceMinRow, _ ) =
            rowRange source

        ( destMinRow, _ ) =
            rowRange dest
    in
    destMinRow - sourceMinRow


fullRows : Int -> List Block -> List Int
fullRows maxColumns blocks =
    let
        ( minRow, maxRow ) =
            rowRange blocks

        rowCounts : List ( Int, Int )
        rowCounts =
            List.range minRow maxRow
                |> List.map
                    (\row ->
                        List.filter (\block -> block.row == row) blocks
                            |> (\bs -> ( row, List.length bs ))
                    )
    in
    List.filter (\( _, count ) -> count == maxColumns) rowCounts
        |> List.map (\( row, _ ) -> row)


removeFullRows : Int -> List Block -> List Block
removeFullRows maxColumns blocks =
    List.foldl removeRow blocks (fullRows maxColumns blocks)


removeRow : Int -> List Block -> List Block
removeRow row blocks =
    let
        remainingBlocks =
            List.filter (\block -> block.row /= row) blocks

        translateIfNeeded block =
            if block.row < row then
                { block | row = block.row + 1 }

            else
                block
    in
    List.map translateIfNeeded remainingBlocks


columnRange : List Block -> ( Int, Int )
columnRange blocks =
    range .col blocks


rowRange : List Block -> ( Int, Int )
rowRange blocks =
    range .row blocks


range : (Block -> Int) -> List Block -> ( Int, Int )
range value blocks =
    let
        values =
            List.map value blocks

        min =
            List.minimum values |> Maybe.withDefault 0

        max =
            List.maximum values |> Maybe.withDefault -1
    in
    ( min, max )


colorToHex : Color -> String
colorToHex color =
    case color of
        Red ->
            "#FD0000"

        Green ->
            "#36C54C"

        Blue ->
            "#3968B0"

        Cyan ->
            "#2EA3F7"

        Orange ->
            "#FA6600"

        Purple ->
            "#CA55C3"

        Yellow ->
            "#F2D00D"

        Gray ->
            "#DDD"
