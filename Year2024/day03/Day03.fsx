#if ! INTERACTIVE 
module Year2024.Day03
#endif

open System.IO
open System.Text.RegularExpressions

let input = $"{__SOURCE_DIRECTORY__}/input.txt"
let example = $"{__SOURCE_DIRECTORY__}/example2.txt"

let listToTuple = function 
    | [ e1; e2 ] -> (e1, e2)
    | _ -> failwith "Not two elements."

let getNumbersInMulMatch (m: Match) = 
    [ 1; 2 ]
    |> List.map (fun i -> Seq.item i m.Groups |> _.Value |> int)
    |> listToTuple

let regexMatches pattern str = Regex.Matches(str, pattern)

let mulRegex = @"mul\((\d+),(\d+)\)"

let curry f (x, y) = f x y 

let part1 () = 
    File.ReadAllText input
    |> regexMatches mulRegex
    |> Seq.map (getNumbersInMulMatch)
    |> Seq.sumBy (curry (*))
    
let part2 () = 
    File.ReadAllText input 
    |> _.Split(@"do()")
    |> Array.map (_.Split(@"don't()").[0])
    |> String.concat "" 
    |> regexMatches mulRegex
    |> Seq.map (getNumbersInMulMatch)
    |> Seq.sumBy (curry (*))

#if INTERACTIVE
printfn $"Part 1: %A{part1 ()}"
printfn $"Part 2: %A{part2 ()}"
#endif
