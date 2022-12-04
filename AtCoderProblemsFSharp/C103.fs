module C103

let n = stdin.ReadLine() |> int
let aArray = stdin.ReadLine().Split() |> Array.map int

aArray |> Seq.map (fun x -> x-1) |> Seq.sum |> stdout.WriteLine