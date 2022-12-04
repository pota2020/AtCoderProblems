module C137

let n = stdin.ReadLine() |> int
let s = seq{for _ in 1.. n -> stdin.ReadLine()}
 
let combi2 x =int64 x * int64 (x-1L) / 2L
s |> Seq.map (fun str -> str.ToCharArray() |> Array.sort |> fun x -> new string(x) )
|> Seq.countBy (fun x -> x)
|> Seq.filter (fun (_, count) -> count > 1)
|> Seq.sumBy (fun (_, count) -> combi2 <| int64 count)
|> string
|> stdout.WriteLine