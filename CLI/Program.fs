open System.Reflection
open System

let tryGetModule (project: string) (moduleName: string) =
    try
        Assembly.Load(project).GetTypes()
        |> Array.tryFind (fun t -> t.FullName = moduleName)
    with :? Collections.Generic.KeyNotFoundException ->
        None

let loadSolutionModule (year: string) (day: string) =
    tryGetModule $"Year{year}" $"Year{year}.Day%02d{int day}"

let printResultForPart (solutionModule: Type) (part: int) =
    printfn $"PART {part}:"

    try
        let methodInfo = solutionModule.GetMethod($"part{part}")
        let result = methodInfo.Invoke(null, [||])

        printfn $"{result}"
    with
    | :? NullReferenceException
    | :? Collections.Generic.KeyNotFoundException -> printfn "NOT SOLVED."
    | :? TargetParameterCountException -> printfn $"(ERROR) Function for part {part} must have no input."

[<EntryPoint>]
let main args =
    match args with
    | [| year; day |] ->
        printfn $"========================="
        printfn $"RESULTS FOR DAY {day} ({year})"

        match loadSolutionModule year day with
        | None -> printfn "No results for that day."
        | Some solutionModule -> List.iter (printResultForPart solutionModule) [ 1; 2 ]

        printfn $"========================="
    | _ -> printfn "Please specify year and date as arguments."

    0
