#if INTERACTIVE 
#load "Utils.fsx"
open Utils
#else
module Year2023.Day09
open Year2023.Utils
#endif

let puzzle = ("2023", "09")
let input = getInput puzzle

(****************************************************************************
 ********************************** Part 1 **********************************
 ****************************************************************************)

let ex1 =
    "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45"

let parse (line: string) =
    line.Split(' ') |> Seq.toList |> List.map int

let stepDiffs ls =
    List.pairwise ls |> List.map (fun (a, b) -> b - a)

let rec repeatUntillZeros ls =
    let sds = stepDiffs ls

    if List.forall ((=) 0) sds then
        [ ls; sds ]
    else
        ls :: repeatUntillZeros sds

let part1 () =
    // ex1.Split('\n')
    readLines input
    |> Seq.map (parse >> repeatUntillZeros >> List.map (List.rev) >> List.sumBy (List.head))
    |> Seq.sum

(****************************************************************************
 ********************************** Part 2 **********************************
 ****************************************************************************)

let extrapolateBack ls =
    List.scan (fun prev cur -> cur - prev) (List.head ls) (List.tail ls)
    |> List.rev
    |> List.head

let part2 () =
    // ex1.Split('\n')
    readLines input
    |> Seq.map (
        parse
        >> repeatUntillZeros
        >> List.rev
        >> List.map (List.head)
        >> extrapolateBack
    )
    |> Seq.sum
