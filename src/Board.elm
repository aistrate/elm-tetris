module Board exposing (..)

import Block exposing (..)
import Dict exposing (Dict)
import Tetromino exposing (..)


type Board
    = Board
        { columns : Int
        , rows : Int
        , occupiedCells : Dict ( Int, Int ) ()
        }


createBoard : Int -> Int -> List Block -> Board
createBoard columns rows bottomBlocks =
    let
        occupiedCells =
            List.map (\block -> ( ( block.col, block.row ), () )) bottomBlocks
                |> Dict.fromList
    in
    Board
        { columns = columns
        , rows = rows
        , occupiedCells = occupiedCells
        }


collision : List Block -> Board -> Bool
collision blocks (Board { columns, rows, occupiedCells }) =
    let
        blockCollision block =
            not (0 <= block.col && block.col < columns && block.row < rows)
                || Dict.member ( block.col, block.row ) occupiedCells
    in
    List.any blockCollision blocks


calculateGhostPiece : List Block -> Board -> List Block
calculateGhostPiece blocks board =
    if not (List.isEmpty blocks) then
        let
            initial =
                List.map (\block -> { block | color = Gray }) blocks

            moveToBottomFrom : List Block -> List Block
            moveToBottomFrom current =
                let
                    next =
                        List.map (\block -> { block | row = block.row + 1 }) current
                in
                if collision next board then
                    current

                else
                    moveToBottomFrom next
        in
        moveToBottomFrom initial

    else
        []


firstViableAlternative : List Translation -> Tetromino -> Board -> Maybe Tetromino
firstViableAlternative translations tetromino board =
    case translations of
        translation :: remainingTranslations ->
            let
                alternative =
                    translateBy translation tetromino
            in
            if not (collision alternative.blocks board) then
                Just alternative

            else
                firstViableAlternative remainingTranslations tetromino board

        [] ->
            Nothing
