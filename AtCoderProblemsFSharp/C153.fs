module C153

/// 数、回数
let n, k = stdin.ReadLine().Split() |> Array.map int64 |> fun [|x1; x2|] -> x1, x2
/// 体力[番号]
let hs = stdin.ReadLine().Split() |> Array.map int64

// nlognまで
/// 上からkまでの値を0にし、他は足し算
hs |> Seq.sortDescending |> Seq.skip (min k n |> int) |> Seq.sum |> stdout.WriteLine