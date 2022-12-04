module C134

open System.IO
open System

let n = stdin.ReadLine() |> int
let a = [|0..n|] |> Array.map (fun x ->if x <> 0 then stdin.ReadLine()|> int else 0)

let maxI, maxA = a |> Array.mapi (fun i x -> i, x) |> Array.maxBy (fun (_ ,x) -> x )
let aRemoved = a |> Array.copy
aRemoved.[maxI]<-0
let nextI, nextA = aRemoved |> Array.mapi (fun i x -> i, x) |> Array.maxBy (fun (_ ,x) -> x )

let sw = new StreamWriter(Console.OpenStandardOutput())
sw.AutoFlush <- false
Console.SetOut(sw)

a.[1..] 
|> Array.map (fun x -> if x = maxA then nextA else maxA )
|> Array.iter stdout.WriteLine
stdout.Flush()
