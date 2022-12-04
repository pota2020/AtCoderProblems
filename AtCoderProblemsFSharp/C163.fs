module C163

open System
open System.IO

let sw = new StreamWriter(Console.OpenStandardOutput())
sw.AutoFlush <- false
Console.SetOut(sw)

let n = stdin.ReadLine() |> int
let a = stdin.ReadLine().Split()

let parentNoDict = a |> Array.countBy id |> dict

{1..n} |> Seq.map (fun x -> parentNoDict.TryGetValue(string x))
|> Seq.map (fun (x1, x2) -> 
    match x1, x2 with
    | true, v -> v
    | false, _ -> 0)
|> Seq.map string
|> Seq.iter stdout.WriteLine

stdout.Flush()