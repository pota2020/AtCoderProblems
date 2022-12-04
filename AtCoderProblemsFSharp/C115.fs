module C115

let [| n; k; |] = stdin.ReadLine().Split() |> Array.map int
let hs = seq{ for i in 1..n -> stdin.ReadLine() |> int}

let sorted = hs |> Seq.sort |> Seq.toArray
[0..n-k] |> Seq.map (fun i -> sorted.[i+k-1] - sorted.[i])
|> Seq.min |> stdout.WriteLine