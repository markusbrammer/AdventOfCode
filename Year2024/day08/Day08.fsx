#if ! INTERACTIVE
module Year2024.Day08
#endif

open System.IO

let input = $"{__SOURCE_DIRECTORY__}/input.txt"
let example = $"{__SOURCE_DIRECTORY__}/example.txt"

let parseAntennaGrid = File.ReadAllLines >> Array.map (Seq.toArray)

let toTuple a b = (a, b)

let collectAntennas antennaGrid =
    let rowFolder row posMap =
        function
        | (_, '.') -> posMap
        | (col, antenna) ->
            let curPos = (row, col)

            let antennaPositions =
                match Map.tryFind antenna posMap with
                | None -> [ curPos ]
                | Some positions -> curPos :: positions

            Map.add antenna antennaPositions posMap

    let colFolder posMap (row, rowArr) =
        Array.mapi toTuple rowArr |> Array.fold (rowFolder row) posMap

    antennaGrid |> Array.mapi toTuple |> Array.fold colFolder Map.empty

let tupleAdd (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)
let tupleDiff (a1, b1) (a2, b2) = (a1 - a2, b1 - b2)

let getImmediateAntinodes isValid pos1 pos2 =
    let posDiff = tupleDiff pos2 pos1

    [ tupleDiff pos1 posDiff; tupleAdd pos2 posDiff ] |> List.filter isValid

let flip f x y = f y x

let getAntinodesWithResonantHarmonics isValid pos1 pos2 =
    let posDiff = tupleDiff pos2 pos1

    let unfolder calcNext =
        function
        | illegalPos when not (isValid illegalPos) -> None
        | pos -> Some(pos, calcNext pos)

    List.unfold (unfolder (flip tupleDiff posDiff)) pos1
    @ List.unfold (unfolder (tupleAdd posDiff)) pos2

let uncurry f (x, y) = f x y

let getUniqueAntinodes getAntinodes isValid antennaPositions =
    List.allPairs antennaPositions antennaPositions
    |> List.filter (uncurry (<>))
    |> List.collect (uncurry (getAntinodes isValid))
    |> Set.ofList

let getIsValid antennaGrid =
    fun (row, col) ->
        let height = Array.length antennaGrid
        let width = Array.length antennaGrid.[0]

        0 <= row && row < height && 0 <= col && col < width

type Harmonics =
    | WithoutResonantHarmonics
    | WithResonantHarmonics

let countUniqueAntinodes harmonics inputPath =
    let antennaGrid = parseAntennaGrid inputPath
    let isValid = getIsValid antennaGrid

    let getAntinodes =
        match harmonics with
        | WithoutResonantHarmonics -> getImmediateAntinodes
        | WithResonantHarmonics -> getAntinodesWithResonantHarmonics

    antennaGrid
    |> collectAntennas
    |> Map.map (fun _ -> getUniqueAntinodes getAntinodes isValid)
    |> Map.values
    |> Set.unionMany
    |> Set.count

let part1 () =
    countUniqueAntinodes WithoutResonantHarmonics input

let part2 () =
    countUniqueAntinodes WithResonantHarmonics input

#if INTERACTIVE
printfn $"Part 1: %A{part1 ()}"
printfn $"Part 2: %A{part2 ()}"
#endif
