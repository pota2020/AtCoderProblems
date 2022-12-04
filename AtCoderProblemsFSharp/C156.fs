module C156

let n = stdin.ReadLine() |> int
let xArray = stdin.ReadLine().Split() |> Array.map int

[ Array.min xArray .. Array.max xArray ]
|> Seq.map (fun p -> xArray |> Seq.map (fun x -> pown (abs (x - p)) 2) |> Seq.sum )
|> Seq.min
|> stdout.WriteLine