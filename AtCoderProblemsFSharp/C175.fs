module C175

// WA
open Microsoft.FSharp.Core.Operators.Checked

let [| X; K; D; |] = stdin.ReadLine().Split() |> Array.map int64

let minAddress = 
    let x, k = 
        abs X
        |> fun x -> if K % 2L = 0L then x, K else (x - D, K - 1L)  

    if k = 0L then x 
    elif x / k - D >= 0L then
        if x - k * D > 0L then x - k * D
        else min (x % (2L * D)) (2L * D - (x % (2L * D)))
    else min (x % (2L * D)) (2L * D - (x % (2L * D)))

minAddress |> stdout.WriteLine

