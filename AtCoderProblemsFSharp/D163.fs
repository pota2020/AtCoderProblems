module D163

// 何個の数を用いるか、そして取り得る範囲はどこからどこまでか、この二つを考える

let n, k = stdin.ReadLine().Split() |> fun [| x1; x2 |] -> int64 x1, int64 x2

/// k=index時の取り得るmin,max
let minMaxArray =
    let array :(int64*int64)[]= Array.zeroCreate (int n + 2)
    let mutable mintmp = 0L
    let mutable maxtmp = 0L
    
    for x in [0L..n] do
        mintmp <- mintmp + int64 x
        array.[int x + 1] <- (mintmp, 0L)
    
    for x in [n .. -1L .. 0L] do
        maxtmp <- maxtmp + x
        array.[int n + 1 - int x] <- ((fst array.[int n + 1 - int x] ), maxtmp)
    
    array

{int k..int n + 1}
|> Seq.map (fun i -> snd minMaxArray.[i] - fst minMaxArray.[i] + 1L)
|> Seq.fold (fun acc x -> (acc + x) % 1_000_000_007L ) 0L
|> stdout.WriteLine
