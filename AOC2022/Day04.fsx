#load "Utils.fsx"
open Utils

let testInputLines = readInputLines "TestDay04.txt"
let puzzleInputLines = readInputLines "InputDay04.txt"

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

let getResultPart1 inputLines =
    getCleaningPairs inputLines
    |> List.filter eitherFullyContains
    |> List.length

getResultPart1 testInputLines
let resultPart1 = getResultPart1 puzzleInputLines

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

let getResultPart2 inputLines =
    getCleaningPairs2 inputLines |> List.filter rangeOverlaps |> List.length

getResultPart2 testInputLines
let resultPart2 = getResultPart2 puzzleInputLines

(* === Print results === *)

printResult 1 <| resultPart1
printResult 2 <| resultPart2
