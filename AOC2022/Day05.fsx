#load "Utils.fsx"

open Utils

open System.Text.RegularExpressions

let testInputLines = readInputLines "TestDay05.txt"
let puzzleInputLines = readInputLines "InputDay05.txt"

(* === PART 1 === *)

let assignInitialCrates (inputLines: string list) =
    let rec assignColumns map =
        function
        | [] -> map
        | row :: isrest -> assignColumns (assignRow 1 map (Seq.toList row)) isrest
    and assignRow i map = function
        | [] -> map
        | '[' :: c :: ']' :: rsrest ->
            match Map.tryFind i map with
            | None -> assignRow (i + 1) (Map.add i [ c ]  map) rsrest
            | Some cs -> assignRow (i + 1) (Map.add i (cs @ [ c ]) map) rsrest 
        | ' ' :: ' ' :: ' ' :: ' ' :: rsrest-> assignRow (i + 1) map rsrest
        | _ :: rsrest -> assignRow i map rsrest
            
    let rx = Regex @"\[[A-Z]\]"
    
    assignColumns Map.empty (List.filter rx.IsMatch inputLines)
        
assignInitialCrates puzzleInputLines

let getCommands (inputLines: string list) =
    let rx = Regex @"move ([0-9]+) from ([0-9]) to ([0-9])"

    List.filter rx.IsMatch inputLines
    |> List.map (fun s ->
        let groups = rx.Match(s).Groups
        (int groups.[1].Value, int groups.[2].Value, int groups.[3].Value))

getCommands puzzleInputLines

let rec move (quantity, stack1, stack2) map =
    match quantity with
    | 0 -> map
    | q ->
        match Map.find stack1 map with
        | [] -> map
        | c1 :: cs1 ->
            let cs2 = Map.find stack2 map
            
            Map.add stack1 cs1 map
            |> Map.add stack2 (c1 :: cs2)
            |> move (q - 1, stack1, stack2)

let resultPart1 inputLines =
    let map = assignInitialCrates inputLines
    let commands = getCommands inputLines

    // Should have used fold as this caused a lot of issues. Reversing list
    // of commands to fix
    List.foldBack move (List.rev commands) map
    |> Map.fold
        (fun crates _ cs ->
            match cs with
            | c :: _ -> crates + string c
            | _ -> crates)
        ""

resultPart1 puzzleInputLines

(* === PART 2 === *)

let move2 (q, s1, s2) map =
    match Map.find s1 map with
    | [] -> map
    | cs1 ->
        let (cs, cs1') = List.splitAt (min q (List.length cs1)) cs1
        Map.add s1 cs1' map |> Map.add s2 (cs @ Map.find s2 map)
        
   
let resultPart2 inputLines =
    let map = assignInitialCrates inputLines
    let commands = getCommands inputLines

    // Should have used fold as this caused a lot of issues. Reversing list
    // of commands to fix
    List.foldBack move2 (List.rev commands) map
    |> Map.fold
        (fun crates _ cs ->
            match cs with
            | c :: _ -> crates + string c
            | _ -> crates)
        ""
resultPart2 puzzleInputLines

(* === Print results === *)

printResult 1 <| resultPart1 puzzleInputLines
printResult 2 <| resultPart2 puzzleInputLines
