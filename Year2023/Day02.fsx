#if INTERACTIVE 
#load "Utils.fsx"
open Utils
#else
module Year2023.Day02
open Year2023.Utils
#endif

open System.Text.RegularExpressions

let puzzle = ("2023", "02")
let input = getInput puzzle

(****************************************************************************
 ********************************** Part 1 **********************************
 ****************************************************************************)

let ex1 =
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

let parseDraw (draw: string) =
    let counts = Map.ofList [ ("blue", 0); ("green", 0); ("red", 0) ]
    let rgx = @"(\d+)\s(green|red|blue)"

    Seq.fold
        (fun cs (m: Match) -> Map.add (m.Groups.[2].Value) (int (m.Groups.[1].Value)) cs)
        counts
        (Regex.Matches(draw, rgx))

let parseGame (line: string) =
    let id = int <| Regex.Match(line, @"Game (\d+)").Groups.[1].Value

    let draws =
        line.Split(':').[1].Split(';') // Don't care about Game number
        |> Seq.map parseDraw

    (id, draws)

let isPossible bag (_, draws) =
    Seq.forall (Map.forall (fun k v -> v <= Map.find k bag)) draws

let part1 () =
    let bag = Map.ofList [ ("blue", 14); ("green", 13); ("red", 12) ]

    readLines input
    |> Seq.map (parseGame)
    |> Seq.filter (isPossible bag)
    |> Seq.sumBy fst

(****************************************************************************
 ********************************** Part 2 **********************************
 ****************************************************************************)

let unionMax m1 m2 =
    Map.fold (fun acc k v -> if Map.find k acc < v then Map.add k v acc else acc) m1 m2

let setPower counts =
    Seq.fold unionMax (Seq.head counts) (Seq.tail counts)
    |> Map.fold (fun pow _ c -> c * pow) 1

let part2 () =
    readLines input
    |> Seq.map (parseGame >> snd)
    |> Seq.sumBy setPower
