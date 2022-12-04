module D165

let a, b, n = stdin.ReadLine().Split() |> fun [| a; b; n|] -> int a, int64 b, int64 n

// x/Bの小数部をβとすると[Aβ]が答え これは1<=n<B-1のとき最大値x=nそれ以外は最大値x=B-1
let beta x = x - floor x

if n < b-1L then float n 
else (float b - 1.0)
|> fun x -> x / float b |> beta |> (*) (float a) |> floor |> string |> stdout.WriteLine