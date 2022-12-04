module C107

let [| total; usedCount |] = stdin.ReadLine().Split() |> Array.map int
let candles = stdin.ReadLine().Split() |> Array.map int

//let distance sortedCandles =
//    min (sortedCandles |> Array.head |> abs) (sortedCandles |> Array.last |> abs)
//    |> (+) ((sortedCandles |> Array.head) - (sortedCandles |> Array.last) |> abs)
let distance (head,last) =
    min (head |> abs) (last |> abs)
    |> (+) (last-head |> abs)

// 配列のコピーにコストがかかっているように思われるので、配列に直にアクセスする
let headLastIPairs =
    [| for i in usedCount-1..total-1 -> i - (usedCount-1), i |]

headLastIPairs
|> Seq.map (fun (i1,i2) -> candles.[i1],candles.[i2])
//|> Seq.windowed usedCount
|> Seq.map distance
|> Seq.min
|> stdout.WriteLine