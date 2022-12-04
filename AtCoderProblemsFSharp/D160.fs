module D160

open System.Linq.Expressions

/// 境界はIn
type InOut =
    | In
    | OutLeft
    | OutRight

// 最短候補は高々2つなので距離をマッピングしていく
let n, x, y = stdin.ReadLine().Split() |> fun arr -> int arr.[0], (int arr.[1]) - 1, (int arr.[2]) - 1

/// aからの距離マップ(距離、個数) (a < n-1)
let distanceMap (a:int) =

    let inOut a =
        if a < x then
            OutLeft
        elif a > y then
            OutRight
        else 
            In

    let length a b =
        match (inOut a, inOut b) with
        | In, In -> min (b - a) (y - b + a - x + 1)
        | In, OutRight  -> min (b - a) (b - y + a - x + 1)
        | OutLeft, In -> min (b - a) (y - b + x - a + 1)
        | OutLeft, OutRight -> b- y + x - a + 1
        | OutLeft, OutLeft | OutRight, OutRight -> b - a

    [a + 1 .. n - 1]
    |> Seq.map (fun b -> length a b)
    |> Seq.countBy (fun x -> x)

let tmp =
    [0..n-2]
    |> Seq.map distanceMap
    |> Seq.collect (fun x -> x)

let result = Array.zeroCreate<int> n

for (length, count) in tmp do
    result.[length] <- result.[length] + count

result.[1..n-1]
|> Seq.iter (stdout.WriteLine)
