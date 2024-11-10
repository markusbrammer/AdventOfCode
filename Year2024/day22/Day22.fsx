#if ! INTERACTIVE 
module Year2024.Day22
#endif

open System.IO

let inputPath = $"{__SOURCE_DIRECTORY__}/input.txt"
let examplePath = $"{__SOURCE_DIRECTORY__}/example.txt"

let part1 () =
    File.ReadAllText inputPath
    
#if INTERACTIVE
part1 ()
#endif

let part2 () = printfn "NOT IMPLEMENTED"

#if INTERACTIVE
part2 ()
#endif

