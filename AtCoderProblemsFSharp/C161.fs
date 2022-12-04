module C161

let n, k = stdin.ReadLine().Split() |> fun [| x; y |] -> int64 x, int64 y

min (n % k) (k - n % k)
|> stdout.WriteLine