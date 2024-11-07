#if INTERACTIVE 
#load "Utils.fsx"
open Utils
#else
module Year2022.Day01
open Year2022.Utils
#endif

let puzzle = ("2022", "01")

let input = getInput puzzle

let sumOfInventories (calorieList: string list) =
    // Add the sum of calories of each inventory to a list of sums. 
    List.fold
        (fun (sums, inventorySum) entry ->
            match entry with
            | "" -> (inventorySum :: sums, 0)
            | s -> (sums, (int s) + inventorySum))
        ([], 0)
        calorieList
    // Remember the last inventory. 
    |> fun (sums, lastInventorySum) -> lastInventorySum :: sums

let part1 () =
    readAllLines input
    |> sumOfInventories
    |> List.max

(* === PART 2 === *)

let part2 () =
    readAllLines input
    |> sumOfInventories
    |> List.sortDescending
    |> List.take 3
    |> List.sum

