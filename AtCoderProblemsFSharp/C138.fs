module C138

let n = stdin.ReadLine()|>int
let vArray = stdin.ReadLine().Split()|>Array.map double

vArray |> Array.sort |> Array.reduce (fun acc f -> (acc + f) / 2.0) |> stdout.WriteLine