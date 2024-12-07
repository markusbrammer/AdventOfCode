#if ! INTERACTIVE
module Year2024.Day07
#endif

open System.IO

let input = $"{__SOURCE_DIRECTORY__}/input.txt"
let example = $"{__SOURCE_DIRECTORY__}/example.txt"

let arrayToTuple =
    function
    | [| e1; e2 |] -> (e1, e2)
    | _ -> failwith "Array doesn't have two elements."

let tupleMap f g (x, y) = (f x, g y)

let parseTestValueAndCalibrations (line: string) =
    line.Split(": ")
    |> arrayToTuple
    |> tupleMap uint64 (_.Split(' ') >> Array.map uint64 >> Array.toList)

let concat n2 n1 = string n1 + string n2 |> uint64

type ConcatOptions =
    | ConcatOn
    | ConcatOff

let getPossibleCalibrationResults concatOpt testValue calibrations =
    let ops =
        match concatOpt with
        | ConcatOff -> [ (+); (*) ]
        | ConcatOn -> [ (+); (*); concat ]

    let folder results calibration =
        match results with
        | [] -> [ calibration ]
        | _ ->
            ops
            |> List.map (fun op -> List.map (op calibration) results |> List.filter ((>=) testValue))
            |> List.concat

    List.fold folder [] calibrations

let canBeTrue concatOpt (testValue, calibrations) =
    getPossibleCalibrationResults concatOpt testValue calibrations
    |> List.contains testValue

let getTotalCalibrationResult concatOpt =
    File.ReadAllLines
    >> Array.map parseTestValueAndCalibrations
    >> Array.filter (canBeTrue concatOpt)
    >> Array.sumBy fst

let part1 () =
    getTotalCalibrationResult ConcatOff input

let part2 () =
    getTotalCalibrationResult ConcatOn input

#if INTERACTIVE
printfn $"Part 1: %A{part1 ()}"
printfn $"Part 2: %A{part2 ()}"
#endif
