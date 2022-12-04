module C102

let n = stdin.ReadLine() |> int
let a = stdin.ReadLine().Split() |> Array.map int

let aCorrected = a |> Seq.mapi (fun i x -> x-(i+1) ) |> Seq.cache
//中央値を見つける
// n->偶数の場合はどちらでもいい
let center = aCorrected |> Seq.sort |> Seq.item ((n-1)/2)

(aCorrected |> Seq.map(fun x -> x - center |> abs) |> Seq.map int64 |> Seq.sum )
|> stdout.WriteLine