#if ! INTERACTIVE
module Year2024.Day01
#endif

open System.IO

printfn $"{__SOURCE_DIRECTORY__}"

let input = $"{__SOURCE_DIRECTORY__}/input.txt"
let example = $"{__SOURCE_DIRECTORY__}/example.txt"

let parseLine (line: string) =
    match line.Split("  ") with
    | [| left; right |] -> int left, int right
    | _ -> failwith ""

let getSortedLocationsLists =
    File.ReadAllLines
    >> Array.map parseLine
    >> Array.toList
    >> List.unzip
    >> fun (left, right) -> List.sort left, List.sort right

let part1 () =
    getSortedLocationsLists input
    ||> List.fold2 (fun dist left right -> dist + abs (left - right)) 0

let rec getSimilarityScore total =
    function
    | _, []
    | [], _ -> total
    | l :: ls, r :: rs when l = r -> getSimilarityScore (total + l) (l :: ls, rs)
    | l :: ls, r :: rs when l < r -> getSimilarityScore total (ls, r :: rs)
    | l :: ls, r :: rs when l > r -> getSimilarityScore total (l :: ls, rs)

let part2 () =
    getSortedLocationsLists input |> getSimilarityScore 0

#if INTERACTIVE
printfn $"Part 1: {part1 ()}"
printfn $"Part 2: {part2 ()}"
#endif
