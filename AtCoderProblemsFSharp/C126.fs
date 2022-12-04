module C126

open System

let [| n; k; |] = stdin.ReadLine().Split() |> Array.map float

seq{1.0..n} |> Seq.map (fun x -> k / x) |> Seq.map (fun x -> match log x / log 2.0 with y when y < 0.0 -> 0.0 | y -> y )
|> Seq.map (fun x -> Math.Ceiling x) |> Seq.map (fun x -> 2.0 ** x) 
|> Seq.sumBy (fun x -> 1.0 / x / n)
|> stdout.WriteLine