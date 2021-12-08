module Aoc.Day1

open Aoc.Utils

let genericSolution (windowSize: int) (s: string) : int =
    getLines s
    |> Seq.map int
    |> Seq.windowed windowSize
    |> Seq.map Array.sum
    |> Seq.pairwise
    |> Seq.map (uncurry (<))
    |> Seq.filter ((=) true)
    |> Seq.length

let part1 = genericSolution 1

let part2 = genericSolution 3
