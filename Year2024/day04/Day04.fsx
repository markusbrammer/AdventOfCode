#if ! INTERACTIVE
module Year2024.Day04
#endif

open System.IO

let input = $"{__SOURCE_DIRECTORY__}/input.txt"
let example = $"{__SOURCE_DIRECTORY__}/example.txt"

let parseAs2DArray inputPath =
    File.ReadAllLines inputPath |> Array.map (Seq.toArray) |> array2D

let isOutOfBounds (arr: 'a[,]) (row, col) =
    row < 0 || Array2D.length1 arr <= row || col < 0 || Array2D.length2 arr <= col

let addTuples (a, b) (x, y) = (a + x, b + y)

let rec findWord (arr: char[,]) direction word row col letter =
    match word with
    | [ letter' ] -> letter = letter'
    | letter' :: wordRest when letter = letter' ->
        match addTuples direction (row, col) with
        | illegalCoord when isOutOfBounds arr illegalCoord -> false
        | (rowNext, colNext) ->
            (rowNext, colNext, arr.[rowNext, colNext])
            |||> findWord arr direction wordRest
    | _ -> false

let countXmas (arr: char[,]) =
    let allDirections =
        [ (-1, 0); (-1, 1); (0, 1); (1, 1); (1, 0); (1, -1); (0, -1); (-1, -1) ]

    let xmas = [ 'X'; 'M'; 'A'; 'S' ]

    let countXmasFrom row col =
        function
        | letter when letter = 'X' ->
            allDirections
            |> List.filter (fun dir -> findWord arr dir xmas row col letter)
            |> List.length
        | _ -> 0

    let mutable sum = 0

    arr
    |> Array2D.iteri (fun row col letter -> sum <- sum + countXmasFrom row col letter)

    sum

let part1 () = parseAs2DArray input |> countXmas

let boolToInt b = if b then 1 else 0

let countXmas2 (arr: char[,]) =
    let isXmas2 (row: int) (col: int) : int =
        match arr.[row, col] with
        | letter when letter = 'A' ->
            let diag1 = set [ arr.[row - 1, col - 1]; arr.[row + 1, col + 1] ]
            let diag2 = set [ arr.[row - 1, col + 1]; arr.[row + 1, col - 1] ]

            (Set.intersect diag1 diag2, set [ 'M'; 'S' ]) ||> (=) |> boolToInt
        | _ -> 0

    let mutable sum = 0

    for row in 1 .. Array2D.length1 arr - 2 do
        for col in 1 .. Array2D.length2 arr - 2 do
            sum <- sum + (isXmas2 row col)

    sum

let part2 () = parseAs2DArray input |> countXmas2

#if INTERACTIVE
printfn $"Part 1: %A{part1 ()}"
printfn $"Part 2: %A{part2 ()}"
#endif
