module C169

let a, b = stdin.ReadLine().Split() |> fun [| a; b;|] -> int64 a, decimal b

decimal a * b |> int64 |> string |> stdout.WriteLine
