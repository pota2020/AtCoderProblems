module C136

let n = stdin.ReadLine() |> int
let h = stdin.ReadLine().Split() |> Array.map int

let rec isAdding (candidate:Option<int>) i (h:array<int>) =
    if i >= n - 1 then "Yes"
    else
        match h.[i],h.[i+1],candidate with
        |x,y,_ when x < y -> isAdding None (i + 1) h
        |x,y,_ when x - y >= 2 -> "No"
        |x,y,z when x - y = 1 && z.IsSome -> "No"
        |x,y,z when x - y = 1 && z.IsNone -> isAdding (Some x) (i + 1) h
        |x,y,z when x = y -> isAdding z (i + 1) h

isAdding None 0 h |> stdout.WriteLine