module ABC121

let [| n; m; |] = stdin.ReadLine().Split() |> Array.map int
let abs = [|for _ in 1..n -> stdin.ReadLine().Split() |> Array.map int64|]

abs |> Array.sort 
|> Array.fold (fun (totalN,totalPrice) [|a;b|] ->
    match totalN with
    | x when x = int64 m -> (x, totalPrice)
    | x when int64 m - b < x  -> (int64 m, totalPrice + a * (int64 m - x)) 
    | x ->(x + b, totalPrice + a * b)) (0L, 0L)
|> fun (_, totalPrice) -> totalPrice
|> stdout.WriteLine