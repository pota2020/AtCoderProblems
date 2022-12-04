module C135

let n = (stdin.ReadLine() |> int)
let aArray = stdin.ReadLine().Split()|>Array.map int64
let bArray = stdin.ReadLine().Split()|>Array.map int64

let countKill (total,surplus) (aBefore,aNext,b) =
    let aBefore = min aBefore surplus
    let modu = max (aBefore + aNext - b) 0L
    let total = total + min b (aBefore + aNext) 
    total,modu

let total,_ = 
    aArray |> Array.pairwise |> Array.zip bArray 
    |>Array.map (fun (b,(a1,a2))->(a1,a2,b))
    |>Array.fold countKill (0L,aArray.[0])

stdout.WriteLine total
