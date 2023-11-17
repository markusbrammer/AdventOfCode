﻿open System.Reflection
open System
open Common

let getModule (project: string) (moduleName: string) : Type =
    Assembly.Load(project).GetTypes() 
    |> Seq.find (fun t -> t.FullName = moduleName)

let loadSolutionModule (year: string) (day: string) =
    getModule $"Year{year}" $"Year{year}.Day{day}"

let tryGetSolution (year: string) (day: string) = 
    try
        (loadSolutionModule year day).GetProperties()
        |> Seq.find (fun p -> p.Name = "solution")
        |> fun p -> p.GetValue(null) :?> Solution 
        |> Some
    with
        | :? Collections.Generic.KeyNotFoundException -> None

let printSolutions (year: string) (day: string) =
    match tryGetSolution year day with 
    | Some s -> printfn $"Part 1:\n{s.part1 ()}\n\nPart 2:\n{s.part2 ()}"
    | None -> printfn $"No solution for year {year}, day {day}"

[<EntryPoint>]
let main args =
    match args with
    | [| year; day |] -> printSolutions year day
    | _ -> printfn "Specify year and date."

    0

// let mi = moduleInfo "AdventOfCode.Year2022.Program"
// let res = mi.GetMethod("f").Invoke(null, [|  |])
// printfn "%A" res

// let allModules () =
//         AppDomain.CurrentDomain.GetAssemblies()
//         |> Seq.map (fun a -> a.GetTypes())
//         |> Seq.map (Seq.map (fun t -> sprintf "%A" t.FullName))
//         |> Seq.filter (Seq.exists (fun s -> s.Contains("ModuleB") || s.Contains("Common")))
//         // |> Seq.concat
//         // |> Seq.filter (fun s -> s.Contains("ModuleB"))
//         |> Seq.concat
//         |> Seq.iter (printfn "%A")
//         // |> printfn "%A"
//         // |> Seq.filter (Seq.exists (fun t -> t.Name.Contains("Year2022")))
