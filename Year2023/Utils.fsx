#if ! INTERACTIVE
module Year2023.Utils
#endif

open System.IO

type Solution<'a, 'b> = {
    part1: unit -> 'a
    part2: unit -> 'b
}

let printSol (year, day) solution = 
    printfn $"Solution to Day %s{day} Year %s{year}:"
    printfn ""
    printfn $"Part 1:\n%A{solution.part1 ()}"
    printfn ""
    printfn $"Part 2:\n%A{solution.part2 ()}"

let readAllLines = File.ReadLines >> Seq.toList

let readLines = File.ReadLines >> seq

let parseEachLine f = File.ReadLines >> Seq.map f

let parseEachLineIndexed f = File.ReadLines >> Seq.mapi f

let charToInt (c: char) = int c - int '0'

let getExample (year, day) = 
    Path.Combine(__SOURCE_DIRECTORY__, "..", $"Year%s{year}", "example", $"day%s{day}.txt")

let getInput (year, day) = 
    Path.Combine(__SOURCE_DIRECTORY__, "..", $"Year%s{year}", "input", $"day%s{day}.txt")

let unitToStrWrap f = 
    fun () -> string f


