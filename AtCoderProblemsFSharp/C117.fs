module C117

let [| n; m; |] = stdin.ReadLine().Split() |> Array.map int
let xs = stdin.ReadLine().Split() |> Array.map int

match m - n <= 0 with
| true -> 0
| false ->
    xs |> Seq.sort |> Seq.pairwise |> Seq.map (fun (x,y) -> y - x) |> Seq.sort 
    |> Seq.take (m-n) |> Seq.sum
|> stdout.WriteLine