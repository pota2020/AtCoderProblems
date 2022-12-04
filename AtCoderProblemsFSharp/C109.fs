module C109

let [| n; x; |] = stdin.ReadLine().Split() |> Array.map int
let xs = stdin.ReadLine().Split() |> Array.map int

let rec gcd x1 x2 =
    let x1,x2 = 
        if x1 >= x2 then x1, x2
        else x2, x1
    if x1 % x2 = 0 then
        x2
    else
        gcd x2 (x1 % x2)

xs |> Array.toList |> fun n -> x::n |> Seq.pairwise |> Seq.map ( fun (before, after) ->abs (after - before))
|> Seq.reduce gcd 
|> stdout.WriteLine