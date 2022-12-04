module C146

let [| a; b; x;|] = stdin.ReadLine().Split() |> Array.map int64

let price n = a * n + b * (string n |> String.length |> int64)

// 高速化のため分割統治法で実行
let maxProduct, maxPrice =
    let rec maxPrice top bottom =
        if top = bottom then
            top, price top
        else
        let mid = float (top+bottom) / 2.0 |> ceil |> int64
        if price mid <= x then
            maxPrice  top mid
        else
            maxPrice (mid-1L) bottom
    if x < a + b then 
        0L, 0L
        else
        maxPrice (pown 10L 9) 1L

maxProduct |> stdout.WriteLine