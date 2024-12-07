#if ! INTERACTIVE
module Year2024.Day05
#endif

open System.IO

let input = $"{__SOURCE_DIRECTORY__}/input.txt"
let example = $"{__SOURCE_DIRECTORY__}/example.txt"

let splitRulesAndUpdatesInput inputLines =
    let splitIndex = List.findIndex ((=) "") inputLines

    List.splitAt splitIndex inputLines |> fun (rs, us) -> rs, List.skip 1 us

let arrayToTuple =
    function
    | [| e1; e2 |] -> (e1, e2)
    | _ -> failwith "Does not contain two elements."

let constructRuleMap beforeAndAfterLists =
    let folder rulesMap before after =
        match Map.tryFind before rulesMap with
        | None -> Map.add before (Set.singleton after) rulesMap
        | Some pages -> Map.add before (Set.add after pages) rulesMap

    beforeAndAfterLists ||> List.fold2 folder Map.empty

let tupleMap f g (x, y) = (f x, g y)

let parse inputPath =
    let (rulesInput, updatesInput) =
        File.ReadAllLines inputPath |> Seq.toList |> splitRulesAndUpdatesInput

    let rules =
        rulesInput
        |> List.map (_.Split('|') >> arrayToTuple)
        |> List.unzip
        |> tupleMap (List.map int) (List.map int)
        |> constructRuleMap

    let updates =
        updatesInput |> List.map (_.Split(',') >> Array.map int >> List.ofArray)

    (rules, updates)

let inOrder rules update =
    let rec inOrderAux prev =
        function
        | [] -> true
        | page :: pages ->
            match Map.tryFind page rules with
            | None -> inOrderAux (Set.add page prev) pages
            | Some pagesAfter when Set.intersect prev pagesAfter |> Set.isEmpty -> inOrderAux (Set.add page prev) pages
            | _ -> false

    inOrderAux Set.empty update

let getMiddlePage update =
    List.item (List.length update / 2) update

let part1 () =
    let rules, updates = parse input

    updates |> List.filter (inOrder rules) |> List.sumBy getMiddlePage

let fixUpdate rules updates =
    let folder fixedUpdate page =
        match Map.tryFind page rules with
        | None -> page :: fixedUpdate
        | Some pagesAfter ->
            let (corrects, wrongs) =
                fixedUpdate |> List.partition (fun page' -> Set.contains page' pagesAfter)

            wrongs @ [ page ] @ corrects

    List.fold folder [] updates

let part2 () =
    let rules, updates = parse input

    updates
    |> List.filter (not << inOrder rules)
    |> List.map (fixUpdate rules)
    |> List.sumBy getMiddlePage

#if INTERACTIVE
printfn $"Part 1: %A{part1 ()}"
printfn $"Part 2: %A{part2 ()}"
#endif
