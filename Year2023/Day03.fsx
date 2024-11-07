#if INTERACTIVE 
#load "Utils.fsx"
open Utils
#else
module Year2023.Day03
open Year2023.Utils
#endif

let puzzle = ("2023", "03")
let input = getInput puzzle

(****************************************************************************
 ********************************** Part 1 **********************************
 ****************************************************************************)

let ex1 =
    "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."

type Pos = int

type Span = Pos * Pos

type Entry =
    | Number of Span * int
    | Symbol of Pos * char

let isDigit c = System.Char.IsDigit(c)

let toNumber span (cs: char list) =
    Number(span, int <| System.String.Concat(cs))

let parseLine (line: string) =
    let rec parseLineAux (i, acc) line =
        match line with
        | [] -> acc
        | c :: _ when isDigit c ->
            let digitList = List.takeWhile isDigit line
            let n = List.length digitList

            parseLineAux (i + n, (toNumber (i, i + n - 1) digitList) :: acc) (List.skip n line)
        | c :: lrest when c <> '.' -> parseLineAux (i + 1, Symbol(i, c) :: acc) lrest
        | _ :: lrest -> parseLineAux (i + 1, acc) lrest

    parseLineAux (0, []) (Seq.toList line)

let parse inputLines =
    let parsedLines = List.map parseLine inputLines

    [] :: parsedLines @ [ [] ]

let inSpan (a, b) =
    function
    | Symbol (x, _) -> a <= x && x <= b
    | Number _ -> false

let isPartsNumber lineAbove lineCurrent lineBelow =
    function
    | Symbol _ -> false
    | Number ((a, b), _) ->
        let span = (a - 1, b + 1)

        List.exists (inSpan span) lineAbove
        || List.exists (inSpan span) lineCurrent
        || List.exists (inSpan span) lineBelow

let rec findPartNumbers =
    function
    | l0 :: l1 :: l2 :: lrest -> List.filter (isPartsNumber l0 l1 l2) l1 :: findPartNumbers (l1 :: l2 :: lrest)
    | _ -> []

let entryVal =
    function
    | Number (_, n) -> n
    | Symbol _ -> 0

let part1 () =
    readLines input
    |> Seq.toList
    |> parse
    |> findPartNumbers
    |> List.concat
    |> List.sumBy entryVal

(****************************************************************************
 ********************************** Part 2 **********************************
 ****************************************************************************)

let isGear =
    function
    | Symbol (_, '*') -> true
    | _ -> false

let isAdjacent e2 e1 =
    match e1, e2 with
    | Number ((a, b), _), Symbol (p, _) -> a - 1 <= p && p <= b + 1
    | _ -> false

let findGearAdjacents lineAbove lineCurrent lineBelow pos =
    let adjs =
        List.filter (isAdjacent pos) lineAbove
        @ List.filter (isAdjacent pos) lineCurrent
          @ List.filter (isAdjacent pos) lineBelow

    match adjs with
    | [ n1; n2 ] -> entryVal n1 * entryVal n2
    | _ -> 0


let rec getGearRatios =
    function
    | l0 :: l1 :: l2 :: lrest ->
        let rowSum =
            List.filter isGear l1 |> List.map (findGearAdjacents l0 l1 l2) |> List.sum

        rowSum + getGearRatios (l1 :: l2 :: lrest)

    | _ -> 0

let part2 () =
    readLines input
    |> Seq.toList
    |> parse
    |> getGearRatios
