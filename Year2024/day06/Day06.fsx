#if ! INTERACTIVE
module Year2024.Day06
#endif

open System.IO
open System.Collections.Generic

let input = $"{__SOURCE_DIRECTORY__}/input.txt"
let example = $"{__SOURCE_DIRECTORY__}/example.txt"

let dirChars = Set.ofList [ '>'; 'v'; '<'; '^' ]

let array2DFindIndex pred (arr: 'a[,]) =
    let rec findIndex (row, col) =
        match arr.[row, col] with
        | elem when pred elem -> (row, col)
        | _ when row < Array2D.length1 arr - 1 -> findIndex (row + 1, col)
        | _ when col < Array2D.length2 arr - 1 -> findIndex (0, col + 1)
        | _ -> failwith "No such element."

    findIndex (0, 0)

let getNextPos (arr: char[,]) (row, col) =
    match arr.[row, col] with
    | '>' -> (row, col + 1)
    | 'v' -> (row + 1, col)
    | '<' -> (row, col - 1)
    | '^' -> (row - 1, col)
    | _ -> failwith $"{(row, col)} is not a guard position."

let turnRight (arr: char[,]) (row, col) =
    match arr.[row, col] with
    | '>' -> 'v'
    | 'v' -> '<'
    | '<' -> '^'
    | '^' -> '>'
    | _ -> failwith "Cannot turn right"

type Result =
    { Visited: Dictionary<int * int, char list>
      HasCycle: bool }

let placeObstacle (labArea: char[,]) obstaclePos =
    match obstaclePos with
    | Some(obstacleRow, obstacleCol) -> labArea.[obstacleRow, obstacleCol] <- '#'
    | None -> ()

let addVisitedDir pos dir (visited: Dictionary<int * int, char list>) =
    visited.[pos] <-
        match visited.TryGetValue(pos) with
        | false, _ -> [ dir ]
        | true, dirs -> dir :: dirs

let getPath obstaclePos (labArea: char[,]) initPos =
    let arr = Array2D.copy labArea
    placeObstacle arr obstaclePos

    let { Visited = visited } as result =
        { Visited = new Dictionary<int * int, char list>()
          HasCycle = false }

    let rec step ((row, col) as pos) =
        let dir = arr.[row, col]

        match getNextPos arr pos with
        | (row', _) when row' < 0 || row' >= Array2D.length1 arr -> result
        | (_, col') when col' < 0 || col' >= Array2D.length2 arr -> result
        | _ when visited.ContainsKey pos && visited.Item pos |> List.contains dir -> { result with HasCycle = true }
        | (row', col') when arr.[row', col'] = '#' ->
            arr.[row, col] <- turnRight arr (row, col)
            step (row, col)
        | (row', col') ->
            arr.[row', col'] <- dir
            addVisitedDir pos dir visited
            step (row', col')

    step initPos

let getLabAreaAndInitPos inputPath =
    let labArea = inputPath |> File.ReadAllLines |> Array.map (Seq.toArray) |> array2D
    let initPos = labArea |> array2DFindIndex (fun elem -> Set.contains elem dirChars)

    (labArea, initPos)

let part1 () =
    getLabAreaAndInitPos input ||> getPath None |> _.Visited |> _.Count

let part2 () =
    let (labArea, initPos) = getLabAreaAndInitPos input
    let { Visited = guardPath } = getPath None labArea initPos

    set guardPath.Keys
    |> Set.remove initPos
    |> Set.filter (fun obstaclePos -> getPath (Some obstaclePos) labArea initPos |> _.HasCycle)
    |> Set.count

#if INTERACTIVE
printfn $"Part 1: %A{part1 ()}"
printfn $"Part 2: %A{part2 ()}"
#endif
