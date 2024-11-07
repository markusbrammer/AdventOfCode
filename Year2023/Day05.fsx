#if INTERACTIVE 
#load "Utils.fsx"
open Utils
#else
module Year2023.Day05
open Year2023.Utils
#endif

// Don't care about "Incomplete pattern matches" warning.
#nowarn "0025"

let puzzle = ("2023", "05")
let input = getInput puzzle

(****************************************************************************
 ********************************** Part 1 **********************************
 ****************************************************************************)

let ex1 =
    "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
0 11 42
49 53 8
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
"

let getSeeds (line: string) =
    line.Split(": ").[1].Split(' ') |> Seq.map uint

let readAlmanacLine m (line: string) =
    let [| dest; src; rng |] = Array.map uint <| line.Split(' ')

    Map.map
        (fun s d ->
            if src <= s && s <= src + rng - 1u then
                dest + s - src
            else
                d)
        m

let rec toMaps m inputLines =
    if Seq.isEmpty inputLines then
        m
    else
        let mapLines = Seq.takeWhile ((<>) "") (Seq.tail inputLines)
        let m' = Map.ofSeq <| Seq.map (fun i -> (i, i)) (Map.values m)

        Seq.skip (Seq.length mapLines + 2) inputLines
        |> toMaps (Seq.fold readAlmanacLine m' mapLines)

let part1 () =
    let inputLines = readLines input
    let seeds = getSeeds (Seq.head inputLines)

    Seq.skip 2 inputLines
    |> toMaps (Map.ofSeq (Seq.map (fun s -> (s, s)) seeds))
    |> Map.values
    |> Seq.min

(****************************************************************************
 ********************************** Part 2 **********************************
 ****************************************************************************)

let rec listPairs =
    function
    | [] -> []
    | x0 :: x1 :: xs -> (x0, x1) :: listPairs xs

type Range = { start: uint; finish: uint }

let seedRanges (line: string) =
    getSeeds line
    |> Seq.toList
    |> listPairs
    |> List.map (fun (s, f) -> { start = s; finish = s + f - 1u })

let splitRange (dest, src, rng) sr =
    let srcFinish = src + rng - 1u
    let destFinish = dest + rng - 1u

    if src <= sr.start && sr.finish <= srcFinish then
        [ { start = dest + (sr.start - src)
            finish = destFinish - (srcFinish - sr.finish) } ]
    elif sr.start < src && src <= sr.finish && sr.finish <= srcFinish then
        [ { start = sr.start; finish = src - 1u }
          { start = dest
            finish = dest + (srcFinish - sr.finish) } ]
    elif src <= sr.start && sr.start <= srcFinish && srcFinish < sr.finish then
        [ { start = dest + (sr.start - src)
            finish = destFinish }
          { start = srcFinish + 1u
            finish = sr.finish } ]
    else
        [ sr ]

let handleNewMappings (dest, src, rng) rns =
    List.map (splitRange (dest, src, rng)) rns

let updateMapping values (line: string) =
    let [| dest; src; rng |] = Array.map uint <| line.Split(' ')

    handleNewMappings (dest, src, rng) values

let rec applyAllMappings values inputLines =
    if Seq.isEmpty inputLines then
        values
    else
        let mapLines = Seq.takeWhile ((<>) "") (Seq.tail inputLines)
        let mappings = Seq.map (updateMapping values) mapLines

        let values' =
            List.mapi
                (fun i v ->
                    match Seq.tryFind (fun m -> List.item i m <> [ v ]) mappings with
                    | None -> [ v ]
                    | Some m -> List.item i m)
                values
            |> List.concat

        Seq.skip (Seq.length mapLines + 2) inputLines |> applyAllMappings values'

let part2 () =
    // let inputLines = seq <| ex1.Split('\n')
    let inputLines = readLines input
    let srs = seedRanges (Seq.head inputLines)

    applyAllMappings srs (Seq.skip 2 inputLines)
    |> List.minBy (fun r -> r.start)
    |> fun r -> r.start
