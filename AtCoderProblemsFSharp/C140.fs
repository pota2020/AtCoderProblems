module C140

let N = stdin.ReadLine() |> int
let B = stdin.ReadLine().Split() |> Array.map int

if B.Length = 1 then
    B.[0]*2 |> stdout.WriteLine
else
(
    B
    |> Array.pairwise
    |> Array.fold (fun acc (a, b) -> acc + min a b) B.[0]
) + B.[N-2]
|> stdout.WriteLine