#if INTERACTIVE 
#load "Utils.fsx"
open Utils
#else
module Year2022.Day06
open Year2022.Utils
#endif

let puzzle = ("2022", "06")
let input = getInput puzzle

(* === PART 1 === *)

let getStartOfPacketMarker (inputLine: string) =
    let rec f i =
        function
        | c0 :: c1 :: c2 :: c3 :: csrest ->
            if List.length (List.distinct [ c0; c1; c2; c3 ]) = 4 then
                i + 3
            else
                f (i + 1) (c1 :: c2 :: c3 :: csrest)
        | _ -> failwith "Cannot find four distinct"

    f 1 <| Seq.toList inputLine

let part1 () =
    match readAllLines input with
    | [ s ] -> getStartOfPacketMarker s
    | _ -> failwith "Wrong input"


(* === PART 2 === *)

let getStartOfPacketMarker2 (inputLine: string) =
    let rec f i cs =
        let first14 = List.take 14 cs

        if List.length (List.distinct first14) = 14 then
            i + 13
        else
            f (i + 1) <| List.tail cs

    f 1 <| Seq.toList inputLine

let part2 () =
    match readAllLines input with
    | [ s ] -> getStartOfPacketMarker2 s
    | _ -> failwith "Wrong input"
