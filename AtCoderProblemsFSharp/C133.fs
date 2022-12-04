module C133

let [| l; m; |] = stdin.ReadLine().Split() |> Array.map int

match seq{l..m} |> Seq.exists (fun x -> x % 2019 = 0 ) with 
|true -> 0
|false -> 
    seq{for i in l%2019..m%2019 do for j in i+1..m%2019 -> i,j} |> Seq.map (fun (x1, x2) -> (x1, x2))
    |> Seq.map (fun (x1,x2) -> x1 * x2 % 2019)
    |> Seq.min
|> stdout.WriteLine
