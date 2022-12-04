module D132

// WA
open System.IO
open System

let m = pown 10L 9 + 5L
let modu = pown 10L 9 + 7L

let [| n; k; |] = stdin.ReadLine().Split() |> Array.map int

//メモ化
let memoize fn =
    let cache = new System.Collections.Generic.Dictionary<_,_>()
    (fun x ->
        match cache.TryGetValue x with
        | true, v -> v
        | false, _ -> 
            let v = fn (x)
            cache.Add(x,v)
            v)

// 逆元
let memoReverce (n:int64) =
    // 繰り返し2乗法（bit）
    let powNM (n:int64, m:int64, modu:int64)=
        let rec f n m modu acc =
            if m=0L then acc
            else
            match m%2L=0L with
            |true  ->f (n*n%modu) (m>>>1) modu acc
            |false ->f (n*n%modu) (m>>>1) modu (acc*n%modu)
        f n m modu 1L
    let g = memoize powNM
    g (n, m, modu)

// Combination (a >= b else 0)
let combination a b =
    if a < b then 0L
    else
    let b = if a - b < b then a - b else b

    let rec memDownF =
        let downF endi =
            match endi with
            | 1L -> 1L
            | x -> memoReverce (int64 x) * memDownF (x-1L) % modu
        memoize downF

    match b with
    | 0L -> 1L
    | 1L -> a
    | b -> 
        // ここはメモ化でさらに高速化できる
        ([a-b+1L..a] |> Seq.reduce (fun total x -> total * x % modu))
        * (memDownF b)

//出力
let sw = new StreamWriter(Console.OpenStandardOutput())
sw.AutoFlush <- false
Console.SetOut(sw)

seq[for i in 1..k -> combination (n-k+1|>int64) (int64 i) * combination (k-1|>int64) (i-1|>int64) % modu]
|> Seq.iter stdout.WriteLine

stdout.Flush()

