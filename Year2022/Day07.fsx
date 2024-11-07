#if INTERACTIVE 
#load "Utils.fsx"
open Utils
#else
module Year2022.Day07
open Year2022.Utils
#endif
open System.Text.RegularExpressions

let puzzle = ("2022", "07")
let input = getInput puzzle

type Element =
    | D of string * Element list
    | F of string * int

let rxCd = Regex @"^\$ cd ([a-zA-Z/.]+)$"
let rxLs = Regex @"^\$ ls$"
let rxFile = Regex @"^([0-9]+) ([a-zA-Z\.]+)$"
let rxDir = Regex @"^dir ([a-zA-Z/]+)$"

let isLsCommand (c: string) = rxLs.IsMatch(c)

let isCdCommand (c: string) = rxCd.IsMatch(c)

let isCommand c = isLsCommand c || isCdCommand c

let getDirName c = rxDir.Match(c).Groups.[1].Value

let getCdName c = rxCd.Match(c).Groups.[1].Value

let toElem (c: string) =
    if rxDir.IsMatch(c) then
        D(getDirName c, [])
    else
        let groups = rxFile.Match(c).Groups
        let filename = groups.[2].Value
        let filesize = int <| groups.[1].Value

        F(filename, filesize)

let rec update dir es =
    match dir with
    | F _ -> failwith "Can only update directories"
    | D (n1, _) ->
        match es with
        | [] -> [ dir ]
        | D (n, _) :: esrest when n = n1 -> dir :: esrest
        | e :: esrest -> e :: update dir esrest

let rec getFileSystem dir cs =
    match dir with
    | F _ -> failwith "Current element must be a directory"
    | D (n, es) ->
        match cs with
        | c :: csrest when isLsCommand c ->
            let output, rest =
                match List.tryFindIndex isCommand csrest with
                | None -> csrest, []
                | Some i -> List.splitAt i csrest

            getFileSystem (D(n, List.map toElem output)) rest
        | c :: csrest when isCdCommand c ->
            match getCdName c with
            | ".." -> D(n, es), csrest
            | n' ->
                let subdir, rest = getFileSystem (D(n', [])) csrest

                getFileSystem (D(n, update subdir es)) rest
        | [] -> D(n, es), []
        | c :: _ -> failwith $"Illegal command: %s{c}"

let generateFileSystem inputLines =
    getFileSystem (D("/", [])) <| List.skip 1 inputLines |> fst

let isFile =
    function
    | F _ -> true
    | D _ -> false

let isDir =
    function
    | D _ -> true
    | F _ -> false

let getFileSize =
    function
    | F (_, s) -> s
    | _ -> failwith "Not a file"

let rec getSizeOfDir =
    function
    | D (n, es) ->
        let sumOfFiles = List.filter isFile es |> List.sumBy getFileSize

        let (dirs, sss) =
            List.fold
                (fun (acc, s) d ->
                    match getSizeOfDir d with
                    | [] -> (acc, s)
                    | (_, sum') :: _ as list -> (list @ acc, s + sum'))
                ([], 0)
                (List.filter isDir es)

        let sum = sumOfFiles + sss

        (n, sum) :: dirs
    | F _ -> failwith "Should only ever deal with directories"


// let dirs = List.filter isDir es |> List.map getSizeOfDir |> List.concat

let part1 () =
    readAllLines input
    |> generateFileSystem
    |> getSizeOfDir
    |> List.filter (snd >> (>) 100000)
    |> List.sumBy snd

let part2 () =
    let sizes =
        readAllLines input
        |> generateFileSystem
        |> getSizeOfDir
    
    let _, total = List.head sizes
    
    List.filter (fun (_, size) -> (70000000 - total) + size > 30000000) sizes
    |> List.sortBy snd
    |> List.head
    |> snd
