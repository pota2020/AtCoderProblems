module C176

let N = stdin.ReadLine() |> int64
let A = stdin.ReadLine().Split() |> Array.map int64

let minTotal = 
    A 
    |> Array.fold 
        (fun (before, total) next ->
            let plusOrZero x = if x >= 0L then x else 0L 
            let step = plusOrZero (before - next)
            step + next, total + step ) 
        (0L, 0L)
    |> snd

stdout.WriteLine (string minTotal)