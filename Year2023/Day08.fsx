#if INTERACTIVE 
#load "Utils.fsx"
open Utils
#else
module Year2023.Day08
open Year2023.Utils
#endif

// Don't care about "Incomplete pattern matches" warning.
#nowarn "0025"

open System.Text.RegularExpressions

let puzzle = ("2023", "08")
let input = getInput puzzle

(****************************************************************************
 ********************************** Part 1 **********************************
 ****************************************************************************)

let ex1 =
    "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)"

let ex2 =
    "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)"

type Instruction = 
    | Left 
    | Right 

let toInstruction = function 
    | 'L' -> Left 
    | 'R' -> Right

let parseInstructions (line: string) = 
    Array.ofSeq line
    |> Array.map toInstruction

let parseElement elementsMap (line: string) = 
    let rgx = @"([A-Z1-9]{3}) = \(([A-Z1-9]{3}), ([A-Z1-9]{3})\)"
    let groups = Regex.Match(line, rgx).Groups
    let key = groups.[1].Value
    let left = groups.[2].Value
    let right = groups.[3].Value

    Map.add key (left, right) elementsMap

let parseElements inputLines = 
    Seq.fold parseElement Map.empty inputLines

let findNext instruction map elem = 
    let (l, r) = Map.find elem map 

    match instruction with 
    | Left -> l
    | Right -> r

let rec findZZZ (i, count) currentElem instructions elems = 
    if currentElem = "ZZZ" then 
        count
    else 
        let next = findNext (Array.item i instructions) elems currentElem
        let i' = (i + 1) % Array.length instructions

        findZZZ (i', count + 1) next instructions elems

let part1 () = 
    // let inputLines = ex1.Split('\n')
    // let inputLines = ex2.Split('\n')
    let inputLines = readLines input
    let instructions = parseInstructions (Seq.head inputLines)
    let elements = parseElements (Seq.skip 2 inputLines)

    findZZZ (0, 0) "AAA" instructions elements

(****************************************************************************
 ********************************** Part 2 **********************************
 ****************************************************************************)

// HINT: The paths are cyclic in a way so that it is only necessary to find 
// the first path count for each path.
//
// Got the hint and main ideas from Jo Van Eyck:
// https://youtu.be/4Sj5GLqIWX0?si=aKbqdkgpCM18PxW4

let ex3 = "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)"

let getStartPositions elements = 
    Map.keys elements 
    |> Seq.filter (fun (s: string) -> s.EndsWith("A"))
    |> Array.ofSeq

let rec findEnd (i, count) instructions elems (currentElem: string) = 
    if currentElem.EndsWith("Z") then 
        count
    else 
        let inst = Array.item i instructions
        let next = findNext inst elems currentElem
        let i' = (i + 1) % (Array.length instructions) 

        findEnd (i', count + 1) instructions elems next 

/// https://stackoverflow.com/a/45532316
let rec gcd a b = 
    match b with 
    | 0UL -> a 
    | _ -> gcd b (a % b)

/// https://stackoverflow.com/a/42472824
let lcmList ls = 
    let rec lcm (res: uint64) = function 
        | [] -> res 
        | e :: lsrest -> lcm ((res * e) / gcd res e) lsrest

    lcm 1UL ls

let part2 () =
    // let inputLines = ex3.Split('\n')
    let inputLines = readLines input
    let instructions = parseInstructions (Seq.head inputLines)
    let elements = parseElements (Seq.skip 2 inputLines)
    let startPos = List.ofArray <| getStartPositions elements

    List.map (findEnd (0,0) instructions elements) startPos
    |> List.map uint64
    |> lcmList
