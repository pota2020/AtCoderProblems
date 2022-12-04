module D159

open System.IO
open System

let sw = new StreamWriter(Console.OpenStandardOutput())
sw.AutoFlush <- false

let _n = stdin.ReadLine()
let _a = stdin.ReadLine().Split() |> Array.map int

let countByA = 
    let res = Array.zeroCreate(200001)
    for num in _a do
        res.[int num]<- res.[int num] + 1
    res

//let c2 n = n * (n - 1L) / 2L
let c2 = seq[0L..200000L] |> Seq.map(fun i ->i*(i-1L)/2L)|>Seq.toArray

// 全部の答えを出して余分な数を引く方針
let ansWhole = 
    seq[1..int _n]
    |> Seq.map (fun x -> c2.[countByA.[x]])
    |> Seq.sum

let unNeeded aNum = 
    let x = int64 countByA.[aNum]
    // xC2 - (x-1)C2 = x-1
    x-1L

/// それぞれの答え
let ansEach aNum = ansWhole - unNeeded aNum |> string

_a
|> Seq.map ansEach
|> Seq.iter stdout.WriteLine

stdout.Flush()
stdout.Close()