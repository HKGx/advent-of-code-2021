module Aoc.Day2

open Aoc.Utils


let generalSolution (f: 'a -> string [] -> 'a) (init: 'a) (content: string) =
    content
    |> getLines
    |> Seq.map (fun s -> s.Split(" "))
    |> Seq.fold f init


let toDirection (horizontal, depth) (next: string []) : int * int =
    match next with
    | [| "forward"; a |] -> (int a, depth)
    | [| "down"; a |] -> (horizontal, depth + int a)
    | [| "up"; a |] -> (horizontal, depth - (int a))
    | _ -> failwith "unreachable"

let part1 s =
    let h, d = generalSolution toDirection (0, 0) s
    h * d



let toAim (horizontal, depth, aim) (next: string []) =

    match next with
    | [| "forward"; a |] -> (horizontal + int a, depth + int a * aim, aim)
    | [| "down"; a |] -> (horizontal, depth, aim + int a)
    | [| "up"; a |] -> (horizontal, depth, aim - int a)
    | _ -> failwith "unreachable"


let part2 s =
    let h, d, _ = generalSolution toAim (0, 0, 0) s
    h * d
