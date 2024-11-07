#if INTERACTIVE 
#load "Utils.fsx"
open Utils
#else
module Year2022.Day04
open Year2022.Utils
#endif


let puzzle = ("2022", "04")
let input = getInput puzzle

(* === PART 1 === *)

type SectionSpan = int * int
type CleaningPair = SectionSpan * SectionSpan

let splitStringToPair (c: char) (s: string) : string * string =
    match s.Split(c) with
    | [| s1; s2 |] -> (s1, s2)
    | _ -> failwith "Cannot split into pair."

let getSectionSpan (spanString: string) : SectionSpan =
    splitStringToPair '-' spanString
    |> fun (firstSection, lastSection) -> (int firstSection, int lastSection)

let collectCleaningPair (inputLine: string) =
    splitStringToPair ',' inputLine
    |> fun (elf1, elf2) -> (getSectionSpan elf1, getSectionSpan elf2)

let getCleaningPairs (inputLines: string list) =
    List.map collectCleaningPair inputLines

let eitherFullyContains (((sa1, sa2), (sb1, sb2)): CleaningPair) =
    (sa1 <= sb1 && sa2 >= sb2) || (sb1 <= sa1 && sb2 >= sa2)

let part1 () =
    readAllLines input
    |> getCleaningPairs
    |> List.filter eitherFullyContains
    |> List.length


(* === PART 2 === *)

type Sections = Set<int>
type CleaningPair2 = Sections * Sections

let getSections ((s1, s2): SectionSpan) = Set.ofList [ s1..s2 ]

let getCleaningPairs2 (inputLines: string list) : CleaningPair2 list =
    List.map
        (collectCleaningPair >> fun (s1, s2) -> (getSections s1, getSections s2))
        inputLines

let rangeOverlaps ((s1, s2): CleaningPair2) =
    not (Set.isEmpty <| Set.intersect s1 s2)

let part2 () =
    readAllLines input
    |> getCleaningPairs2
    |> List.filter rangeOverlaps
    |> List.length
