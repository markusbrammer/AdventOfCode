#if INTERACTIVE 
#load "Utils.fsx"
open Utils
#else
module Year2022.Day09
open Year2022.Utils
#endif

open System.Text.RegularExpressions

let puzzle = ("2022", "09")
let input = getInput puzzle

type Direction =
    | Up
    | Down
    | Left
    | Right

type Motion = Direction * int

let toDirection =
    function
    | "U" -> Up
    | "D" -> Down
    | "R" -> Right
    | "L" -> Left
    | c -> failwith $"%s{c} is not a direction"

let motionRegex = Regex @"^([RULD]) ([0-9]+)$"

let getMotion (s: string) : Motion =
    let regexGroups = motionRegex.Match(s).Groups
    let direction = toDirection <| regexGroups.[1].Value
    let steps = int <| regexGroups.[2].Value

    (direction, steps)

let parseLine = getMotion

let parse = parseEachLine parseLine >> Seq.toList

type Position = int * int

type State1 =
    { positions: Set<Position>
      headPos: Position
      tailPos: Position }

let rec move1
    ({ positions = ps
       headPos = hp
       tailPos = ht } as state)
    ((d, s): Motion)
    : State1 =
    match s with
    | 0 -> state
    | _ ->
        let hp' = moveHeadOnce hp (d, s)
        let ht' = moveTail ht hp'

        move1
            { positions = Set.add ht' ps
              headPos = hp'
              tailPos = ht' }
            (d, s - 1)

and moveHeadOnce ((x, y): Position) ((d, _): Motion) : Position =
    match d with
    | Up -> (x, y + 1)
    | Down -> (x, y - 1)
    | Left -> (x - 1, y)
    | Right -> (x + 1, y)

and moveTail ((xt, yt): Position) ((xh, yh): Position) : Position =
    let xdiff = xh - xt
    let ydiff = yh - yt

    match abs <| xdiff, abs <| ydiff with
    | xdiffa, ydiffa when xdiffa <= 1 && ydiffa <= 1 -> (xt, yt)
    | _ -> (xt + sign xdiff, yt + sign ydiff)

let part1 () =
    parse input
    |> List.fold
        move1
        { positions = set [ (0, 0) ]
          headPos = (0, 0)
          tailPos = (0, 0) }
    |> fun state -> Set.count state.positions

type State2 =
    { positions: Set<Position>
      headPos: Position
      tailPoss: List<Position> }

let rec move2
    ({ positions = ps
       headPos = hp
       tailPoss = hts } as state)
    ((d, s): Motion)
    : State2 =
    match s with
    | 0 -> state
    | _ ->
        let hp' = moveHeadOnce hp (d, s)
        let hts' = moveTails hts hp'

        move2
            { positions = Set.add (List.last hts') ps
              headPos = hp'
              tailPoss = hts' }
            (d, s - 1)


and moveTails hts (xh, yh) =
    match hts with
    | [] -> []
    | (xt, yt) :: htsrest ->
        moveTail (xt, yt) (xh, yh)
        |> fun tailHead -> tailHead :: moveTails htsrest tailHead

let part2 () =
    parse input
    |> List.fold
        move2
        { positions = set [ (0, 0) ]
          headPos = (0, 0)
          tailPoss = List.init 9 (fun _ -> (0, 0)) }
    |> fun state -> Set.count state.positions
