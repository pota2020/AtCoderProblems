module C141

open System.IO
open System

let [| n; k; q; |] = stdin.ReadLine().Split() |> Array.map int
let a = [| for _ in 1..q -> stdin.ReadLine() |> int |]

//最初にk-q持ち点で、正解で一点、最後に0点以下は消える
let pointArray = [|0..n|] |> Array.map (fun _ -> k-q )
do a |> Array.countBy (fun x -> x ) |> Array.iter (fun (i, x) -> pointArray.[i] <- pointArray.[i] + x)

// 出力
let sw = new StreamWriter(Console.OpenStandardOutput())
sw.AutoFlush <- false
Console.SetOut(sw)

pointArray.[1..n] 
|> Array.map (fun x ->
    match x > 0 with
    |true -> "Yes"
    |false -> "No")
|> Array.iter (stdout.WriteLine)

stdout.Flush()

