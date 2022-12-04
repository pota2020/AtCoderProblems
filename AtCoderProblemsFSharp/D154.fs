module D154

let n, k = stdin.ReadLine().Split() |> fun array -> int array.[0], int array.[1]
let pArray = stdin.ReadLine().Split() |> Array.map int

// まずは一列ごとに期待値を求め、その後chunkごとの足し算（畳みこんで、計算量を節約する）を行い、最後にmaxをもとめる
let expArray =
    pArray
    |> Array.map (fun p -> float (1 + p) / 2.0 )

let totalSeq =
    Seq.unfold (
            fun (i,totalBefore) ->
                if i = 0 then
                    let total = Seq.sum expArray.[0..k-1]
                    Some( total, (i + 1, total) )
                elif i = n - k + 1 then
                    None
                else
                    let total = totalBefore - expArray.[i-1] + expArray.[i+k-1]
                    Some( total, (i+1, total))
        )
        (0, 0.0)

totalSeq
|> Seq.max
|> stdout.WriteLine
