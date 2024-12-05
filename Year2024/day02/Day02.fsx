#if ! INTERACTIVE
module Year2024.Day02
#endif

open System.IO

let input = $"{__SOURCE_DIRECTORY__}/input.txt"
let example = $"{__SOURCE_DIRECTORY__}/example.txt"

let parseReport (report: string) =
    report.Split(" ") |> Array.map int |> Array.toList

type Ordering =
    | Dec
    | Inc

let isSafeDiff ordering level1 level2 =
    let diff =
        match ordering with
        | Dec -> level1 - level2
        | Inc -> level2 - level1

    1 <= diff && diff <= 3

let prependOptional opt ls =
    match opt with
    | Some l -> l :: ls
    | None -> ls

let rec isSafe ordering allowDampener previous =
    function
    | []
    | [ _ ] -> true
    | l1 :: l2 :: lsrest when isSafeDiff ordering l1 l2 -> l2 :: lsrest |> isSafe ordering allowDampener (Some l1)
    | l1 :: l2 :: lsrest when allowDampener ->
        [ l1 :: lsrest; l2 :: lsrest ]
        |> List.map (prependOptional previous)
        |> List.exists (isSafe ordering false None)
    | _ -> false

let isSafe1 report =
    isSafe Dec false None report || isSafe Inc false None report

let boolToInt b = if b then 1 else 0

let part1 () =
    File.ReadAllLines input
    |> Array.map (parseReport >> isSafe1)
    |> Array.sumBy boolToInt

let isSafe2 report =
    isSafe Dec true None report || isSafe Inc true None report

let part2 () =
    File.ReadAllLines input
    |> Array.map (parseReport >> isSafe2)
    |> Array.sumBy boolToInt

#if INTERACTIVE
printfn $"Part 1: %A{part1 ()}"
printfn $"Part 2: %A{part2 ()}"
#endif
