module C160

// 最長距離を求める。並び替えて、端だけ特別処理
let k, n = stdin.ReadLine().Split() |> fun x -> int x.[0], int x.[1]
let aArrSorted = stdin.ReadLine().Split() |> Array.map (int) |> Array.sort

let mightA =
    aArrSorted
    |> Seq.pairwise
    |> Seq.map (fun (a,b) -> b-a)
    |> Seq.max

let mightB = 
    aArrSorted.[0] + (k - (aArrSorted |> Array.last))

max mightA mightB
|> fun x -> k - x
|> stdout.WriteLine