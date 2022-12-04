module C155

let n = stdin.ReadLine() |> int
let sArray = [|
    for _ in 1..n -> stdin.ReadLine()
|]

let counted =
    sArray 
    |> Seq.countBy (fun x -> x)
    |> Seq.cache

let max = counted |> Seq.maxBy (fun (str, i) -> i) |> snd

counted 
|> Seq.groupBy (fun (str, i) -> i) 
|> Seq.filter (fun (i, seq) -> i = max)
|> Seq.collect (fun (i, seq) -> seq)
|> Seq.map (fun (str, i) -> str)
|> Seq.sort
|> Seq.iter (fun str -> stdout.WriteLine str)
