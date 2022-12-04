module C123

open System

let n = stdin.ReadLine() |> int64
let ins = [|for _ in 1..5 -> stdin.ReadLine() |> int64 |]

float n / float (Array.min ins) |> Math.Ceiling |> int64 |> (+) 4L |> stdout.WriteLine 
