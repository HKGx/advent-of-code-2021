module Aoc.Program


open Aoc.Utils







let day part1 part2 = Map [ (1, part1); (2, part2) ]

let days =
    Map [ (1, day Day1.part1 Day1.part2)
          (2, day Day2.part1 Day2.part2) ]

let run (day: int) (part: int) (content: string) : unit =
    (printfn "%A" << days.[day].[part]) content

[<EntryPoint>]
let main argv : int =
    assertion <@ argv.Length >= 1 @> "You have to provide day!"
    assertion <@ argv.Length = 2 @> "You have to provide part!"
    assertion <@ isNumeric argv.[0] @> "Day must be an integer!"
    assertion <@ isNumeric argv.[1] @> "Part must be an integer"
    let day = argv.[0] |> int
    let part = argv.[1] |> int
    let day_range = [ 1 .. days.Count ]
    let part_range = [ 1 .. 2 ]
    assertion <@ List.contains day day_range @> "Invalid day!"
    assertion <@ List.contains part part_range @> "Invalid part!"
    let input_path = $"../inputs/input{day}.txt"
    assertion <@ System.IO.File.Exists(input_path) @> $"Expected {input_path} to exist!"
    let content = System.IO.File.ReadAllText(input_path)
    run day part content

    0
