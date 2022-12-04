module C167

open System

let n, m, x = stdin.ReadLine().Split() |> fun [| n; m; x |] -> int n, int m, int x
let ca = 
    [|
        for _ in 0.. n-1 do
            yield stdin.ReadLine().Split()|> Array.map int
    |]

// nが小さいので総当たりで求める
let total (str:string) =
    let arraySum arr1 arr2 = Array.map2 (fun x y -> x+y) arr1 arr2 

    str.ToCharArray() |> Array.rev |> Array.mapi (fun i x -> if x = '1' then Some(ca.[i]) else None)
    |> Array.choose (id) |>Array.reduce (arraySum)


{1 .. pown 2 n - 1} |> Seq.map (fun x -> Convert.ToString(x,2)) |> Seq.map total
|> Seq.filter (fun array -> array.[1..] |> Array.forall (fun e -> e >= x))
|> fun s -> 
    if Seq.isEmpty s then -1
    else s |> Seq.minBy (fun array -> array.[0]) |> fun array -> array.[0]
|> stdout.WriteLine