module C132

let n = stdin.ReadLine() |> int
let d = stdin.ReadLine().Split() |> Array.map int

let sorted = d |> Array.sort
sorted.[n/2] - sorted.[n/2-1] |> stdout.WriteLine