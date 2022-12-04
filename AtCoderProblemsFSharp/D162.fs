module D162

let n = int (stdin.ReadLine())
let s = stdin.ReadLine().ToCharArray()

// ri*gi*bi-(j-i=k-jなるもの)

let countby = 
    s |> Seq.countBy id

let waste =
    let mutable cnt = 0
    for i in 0..n-1 do
        for j in i+1..n-1 do
            let k = j * 2 - i 
            if
                k <= n - 1
                && s.[i] <> s.[j] 
                && s.[i] <> s.[k]
                && s.[j] <> s.[k]
                then
                    cnt <- cnt + 1
    cnt

countby
|> dict
|> fun dic -> (dic.TryGetValue('R') |> snd |> int64) * (dic.TryGetValue('G') |> snd |> int64) * (dic.TryGetValue('B') |> snd |> int64)
|> fun x -> x - int64 waste
|> string
|> stdout.WriteLine