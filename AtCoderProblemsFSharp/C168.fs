module C168

let n, k = stdin.ReadLine().Split() |> fun [| n; k|] -> int n, int64 k

// 配列のインデックス表記を使って辞書を作る
let a = stdin.ReadLine().Split() |> Array.map int |> Array.append [|0|] 

let move (max:int64) =
    let rec move (i, count, mementoIntCount:Map<int,int64 >) = 
        if count >= max then i, count,mementoIntCount
        elif Map.containsKey a.[i] mementoIntCount then // 計算量を節約するために、再び同じ場所を通ったときに経路を省略する
            let length = count - (Map.find a.[i] mementoIntCount) + 1L 
            let count = ((max - count - 1L) / length) * length + count 
            move (a.[i], count + 1L, mementoIntCount )             
        else move (a.[i], count + 1L, Map.add a.[i] (count+1L) mementoIntCount )
    move (1, 0L, Map [1,0L])

move k |>fun (x, _, _) -> string x  |> stdout.WriteLine