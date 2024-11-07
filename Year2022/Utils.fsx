#if ! INTERACTIVE
module Year2022.Utils
#endif

open System.IO

let readAllLines = File.ReadLines >> Seq.toList

let readLines = File.ReadLines >> seq

let parseEachLine f = File.ReadLines >> Seq.map f

let parseEachLineIndexed f = File.ReadLines >> Seq.mapi f

let charToInt (c: char) = int c - int '0'

let getExample (year, day) = 
    Path.Combine(__SOURCE_DIRECTORY__, "..", $"Year%s{year}", "example", $"day%s{day}.txt")

let getInput (year, day) = 
    Path.Combine(__SOURCE_DIRECTORY__, "..", $"Year%s{year}", "input", $"day%s{day}.txt")
