module D138

// WA
open System.IO
open System

let [| n; q; |] = stdin.ReadLine().Split() |> Array.map int
let abArray:int[][] = [|for _ in 1..n-1 -> stdin.ReadLine().Split() |> Array.map int |]
let pxArray:int[][] = [|for _ in 1..q -> stdin.ReadLine().Split() |> Array.map int |]

let mapWithA =
    abArray |> Seq.groupBy (fun ab -> ab.[0]) 
    |> Seq.map (fun (i,array) ->i, array |> Seq.collect (fun x -> x) |> Set.ofSeq)
    |> Map.ofSeq

let mapWithB = 
    abArray |> Seq.groupBy (fun ab -> ab.[1]) 
    |> Seq.map (fun (i,array) ->i, array |> Seq.collect (fun x -> x) |> Set.ofSeq)
    |> Map.ofSeq

let pxMap = pxArray |> Seq.map (fun [| p; x; |] -> p,x) |> Seq.groupBy (fun (p,x) -> p) |> Seq.map (fun (p, pxSeq) -> p, pxSeq |> Seq.sumBy (fun (p, x) -> x)) |>Map.ofSeq

let findNext key =
    if mapWithA.ContainsKey(key) then mapWithA.[key] else set[]
    |> Set.union (if mapWithA.ContainsKey(key) then mapWithA.[key] else set[])
    |> Set.remove key

let rec countUp total i =  seq{
    let total = total + if pxMap.ContainsKey(i) then pxMap.[i] else 0
    yield i, total
    for i in findNext i do yield! countUp total i 
}

//出力
let sw = new StreamWriter(Console.OpenStandardOutput())
sw.AutoFlush <- false
Console.SetOut(sw)

countUp 0 1 
|> Seq.sort
|> Seq.map (fun (i,total) -> string total + " ")
|> Seq.iter stdout.Write

stdout.WriteLine()
stdout.Flush()