#if INTERACTIVE 
#load "Utils.fsx"
open Utils
#else
module Year2023.Day04
open Year2023.Utils
#endif

let puzzle = ("2023", "04")
let input = getInput puzzle

(****************************************************************************
 ********************************** Part 1 **********************************
 ****************************************************************************)

let ex1 = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

let parseNumbers (s: string) = 
    s.Split(' ')
    |> Array.filter ((<>) "")
    |> Array.map int 

let getWinningAndNumbers (card: string) = 
    let allNumbers = (card.Split(':')).[1]

    allNumbers.Split('|') 
    |> Array.map parseNumbers
    |> fun arr -> arr.[0], arr.[1]

let getMatches (winning, numbers) = 
    Array.filter (fun n -> Array.contains n winning) numbers
    |> Array.length 

let cardPoints (card: string) = 
    let matches = getMatches (getWinningAndNumbers card)

    if matches > 0 then 
        1 * int (2. ** float (matches - 1))
    else 
        0 

let part1 () = 
    // ex1.Split('\n')
    System.IO.File.ReadAllLines input
    |> Array.map cardPoints
    |> Array.sum 

(****************************************************************************
 ********************************** Part 2 **********************************
 ****************************************************************************)

let rec countCopies i (arr: (int * int) array) = 
    if i >= Array.length arr then 
        arr
    else
        let (n, c) = arr.[i]

        arr
        |> Array.mapi (fun j (nj, cj) -> if i < j && j <= i + n then (nj, cj + c * 1) else (nj, cj)) 
        |> countCopies (i + 1) 

let part2 () = 
    // ex1.Split('\n')
    System.IO.File.ReadAllLines input
    |> Array.map (getWinningAndNumbers >> getMatches >> fun n -> (n, 1))
    |> countCopies 0
    |> Array.sumBy snd
