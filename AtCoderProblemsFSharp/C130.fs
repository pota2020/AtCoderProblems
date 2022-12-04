module C130

let [| w; h; x; y|] = stdin.ReadLine().Split() |> Array.map int64 

let hasPlural =
    match w % 2L, h % 2L with
    |1L,_ | _, 1L -> 0
    | _ ,_ ->
        match (x,y) = (w/2L, h/2L) with
        |false -> 0
        |true -> 1

w * h |> float |> fun x -> x / 2.0 |>fun x -> string x + " " + string hasPlural
|> stdout.WriteLine