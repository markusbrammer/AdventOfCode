#if INTERACTIVE 
#load "Utils.fsx"
open Utils
#else
module Year2022.Day10
open Year2022.Utils
#endif

open System.Text.RegularExpressions

let puzzle = ("2022", "10")
let input = getInput puzzle

type Operation =
    | NoOp
    | AddX of int

let noopRegex = Regex @"^noop$"
let addxRegex = Regex @"^addx (-?[0-9]+)$"

let toOperation (str: string) =
    match str with 
    | s when noopRegex.IsMatch(s) -> NoOp
    | s when addxRegex.IsMatch(s) -> addxRegex.Match(s).Groups.[1].Value |> int |> AddX
    | s -> failwith $"%s{s} not a valid operation"

let parseLine = toOperation

let parse = parseEachLine parseLine >> Seq.toList

let exec registerHistory op =
    let x =
        match registerHistory with
        | [] -> failwith "The register history should never be empty"
        | head :: _ -> head

    match op with
    | NoOp -> x :: registerHistory
    | AddX value -> x + value :: x :: registerHistory

let part1 () =
    let registerHistory =
        parse input
        |> List.fold exec [ 1 ]
        |> List.rev

    List.fold (fun sum cycle -> sum + cycle * List.item (cycle - 1) registerHistory) 0 [ 20; 60; 100; 140; 180; 220 ]

let fillCrtRow registerHistoryChunk =
    let spriteOverlaps cycle spritePosition = abs (spritePosition - (cycle - 1)) <= 1

    let rec fillRow cycle =
        function
        | [] -> ""
        | spritePosition :: xrest when spriteOverlaps cycle spritePosition -> "# " + fillRow (cycle + 1) xrest
        | _ :: xrest -> ". " + fillRow (cycle + 1) xrest

    fillRow 1 registerHistoryChunk

let part2 () =
    parse input
    |> List.fold exec [ 1 ]
    |> List.rev
    |> List.chunkBySize 40
    |> List.take 6
    |> List.map fillCrtRow
    |> String.concat "\n"
    |> (+) "FGCUZREC: \n"
