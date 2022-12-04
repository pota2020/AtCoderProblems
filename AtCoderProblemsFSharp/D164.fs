module D164

// WA
let s = stdin.ReadLine()

// 1%2019,10%2019..10^671%2019を取って文字を数値に置き換える(フェルマーの小定理より)
let mod2019Array =
    let ans = Array.zeroCreate 672
    ans.[0] <- 1
    for i in 1..671 do
        ans.[i] <- ans.[i-1] * 10 % 2019
    ans

let countTotal (s:string)= 
    let allCand = seq{for i in 0..s.Length-1 do for j in i..s.Length-1 -> i, j}
    let getString cand = s.[fst cand .. snd cand]
    let sumMod2019 (str:string) = 
        str.ToCharArray() |> Array.mapi (fun i c -> (str.Length-i-1) % 672, int c - 48) 
        |> Array.map (fun (i,num) -> mod2019Array.[i] * num |> int64) |> Array.sum

    allCand |> Seq.map getString |> Seq.map sumMod2019 |> Seq.toArray |> Seq.filter (fun total -> total % 2019L = 0L) |> Seq.length

countTotal s |> stdout.WriteLine