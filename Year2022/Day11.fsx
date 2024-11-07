#if INTERACTIVE 
#load "Utils.fsx"
open Utils
#else
module Year2022.Day11
open Year2022.Utils
#endif

open System.IO
open System.Text.RegularExpressions

let puzzle = ("2022", "11")
let input = getInput puzzle

type Monkey =
    { items: uint64 list
      operation: uint64 -> uint64
      divisibleBy: uint64
      targetTrue: int
      targetFalse: int
      inspections: uint64 }

let monkeyRegex =
    @"Monkey ([0-9]+):\n"
    + "  Starting items: (.+)\n"
    + "  Operation: (new = .+)\n"
    + "  Test: divisible by ([0-9]+)\n"
    + "    If true: throw to monkey ([0-9]+)\n"
    + "    If false: throw to monkey ([0-9]+)"
    |> Regex

let toOperation (s: string) =
    let operationRegex = Regex @"new = old ([+*]) (old|[0-9]+)"
    let groups = operationRegex.Match(s).Groups

    let op =
        match groups.[1].Value with
        | "+" -> (+)
        | "*" -> (*)
        | o -> failwith $"%s{o} not a valid operation."

    fun x ->
        match groups.[2].Value with
        | "old" -> op x x
        | n -> op x <| uint64 n

let toMonkey (regexMatch: Match) =
    let groups = regexMatch.Groups

    { items = groups.[2].Value.Split(", ") |> Array.toList |> List.map uint64
      operation = toOperation groups.[3].Value
      divisibleBy = uint64 groups.[4].Value
      targetTrue = int groups.[5].Value
      targetFalse = int groups.[6].Value
      inspections = 0UL }

let parse input =
    File.ReadAllText input |> monkeyRegex.Matches |> Seq.map toMonkey |> Seq.toList

let addItem item (m: Monkey) =
    { items = m.items @ [ item ]
      operation = m.operation
      divisibleBy = m.divisibleBy
      targetTrue = m.targetTrue
      targetFalse = m.targetFalse
      inspections = m.inspections }

let rec monkeyInspect f monkeyNumber (ms: Monkey list) =
    let m = List.item monkeyNumber ms

    match m.items with
    | [] -> ms
    | i :: is ->
        let m' =
            { items = is
              operation = m.operation
              divisibleBy = m.divisibleBy
              targetTrue = m.targetTrue
              targetFalse = m.targetFalse
              inspections = m.inspections + 1UL }

        let i' = m.operation i |> f

        let target =
            if i' % m.divisibleBy = 0UL then
                m.targetTrue
            else
                m.targetFalse

        let mTarget = List.item target ms |> addItem i'

        let ms' = List.updateAt monkeyNumber m' ms |> List.updateAt target mTarget

        monkeyInspect f monkeyNumber ms'

let rec runRound f index (ms: Monkey list) =
    if index < List.length ms then
        monkeyInspect f index ms |> runRound f (index + 1)
    else
        ms

let rec runRounds f numberOfRounds (ms: Monkey list) =
    match numberOfRounds with
    | 0 -> ms
    | n -> runRounds f (n - 1) <| runRound f 0 ms

let part1 () =
    parse input
    |> runRounds (fun x -> x / 3UL) 20
    |> List.map (fun m -> m.inspections)
    |> List.sortDescending
    |> List.take 2
    |> List.fold (*) 1UL

let part2 () =
    let parsedInput = parse input
    let lcm =
        parsedInput
        |> List.map (fun (m: Monkey) -> m.divisibleBy)
        |> List.fold (*) 1UL

    parsedInput
    |> runRounds (fun x -> x % lcm) 10000
    |> List.map (fun m -> m.inspections)
    |> List.sortDescending
    |> List.take 2
    |> List.fold (*) 1UL
