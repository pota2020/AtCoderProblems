module D169

let n = stdin.ReadLine() |> int64

// 素因数分解した後の各要素を1+2+..となるようにmapしてやれば答え

/// xの素因数分解
let pf (x:int64) :int64[]=

    if x = 1L then Array.empty
    else
    let unfolder (dived, div) =
        if div > dived then
            None
        elif div * div > dived then
            Some(Some(dived), (dived, dived + 1L))
        elif dived % div = 0L then
            Some(Some(div), (dived / div, div))
        else 
            Some(None, (dived, div + 1L))
    Array.unfold unfolder (x,2L)
    |> Array.choose id

/// param x return m when x>=1+2+..+m
let stepCount x =
    let mutable i = 1
    let mutable acc = 0
    while x >= acc do 
        acc <- acc + i
        i <- i + 1
    i - 2 


pf n |> Array.countBy id |> Array.map snd |> Array.map stepCount|> Array.sum |> stdout.WriteLine