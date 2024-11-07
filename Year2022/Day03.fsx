#if INTERACTIVE 
#load "Utils.fsx"
open Utils
#else
module Year2022.Day03
open Year2022.Utils
#endif

let puzzle = ("2022", "03")
let input = getInput puzzle

(* === PART 1 === *)

type Compartment = char list

type Rucksack = Compartment * Compartment

let splitIntoCompartments (inputLines: string list) : Rucksack list =
    List.map
        (Seq.toList >> fun rs -> List.splitAt (List.length rs / 2) rs)
        inputLines

let collectDuplicates ((smallC, largeC)) =
    List.distinct <| List.filter (fun item -> List.contains item largeC) smallC

let collectAllDuplicates (rs: Rucksack list) = List.collect collectDuplicates rs

// https://stackoverflow.com/a/20045091
let charToIntIndex (c: char) =
    if not <| c.Equals(System.Char.ToUpper(c)) then
        (int c) % 32
    else
        (int c) % 32 + 26

let part1 () =
    readAllLines input
    |> splitIntoCompartments
    |> collectAllDuplicates
    |> List.map charToIntIndex
    |> List.sum

(* === PART 2 === *)

type RucksackItems = Set<char>
type Group = RucksackItems * RucksackItems * RucksackItems

let getRucksacks (inputLines: string list) : RucksackItems list =
    List.map (Seq.toList >> Set.ofList) inputLines

let rec getGroups (rs: RucksackItems list) =
    match rs with
    | [] -> []
    | r1 :: r2 :: r3 :: rsrest -> (r1, r2, r3) :: getGroups rsrest
    | _ -> failwith "A group of size < 3"

let findGroupIntersect ((r1, r2, r3): Group) =
    match Set.toList <| Set.intersectMany [ r1; r2; r3 ] with
    | [ c ] -> c
    | _ -> failwith "Multiple common items"

let part2 () =
    readAllLines input
    |> getRucksacks 
    |> getGroups
    |> List.map (findGroupIntersect >> charToIntIndex)
    |> List.sum

