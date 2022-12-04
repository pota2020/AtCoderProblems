module D143

let n = stdin.ReadLine() |> int
let ls = stdin.ReadLine().Split() |> Array.map int

let sorted = ls |> Array.sort

//メモ化
let memoize fn =
    let cache = new System.Collections.Generic.Dictionary<_,_>()
    (fun x ->
        match cache.TryGetValue x with
        | true, v -> v
        | false, _ -> 
            let v = fn (x)
            cache.Add(x,v)
            v)

let searchC sum = 
    sorted |>Array.tryFindIndex (fun x -> x >= sum ) 
    |> fun i ->
        match i with
        |None -> n-1
        |Some(i) -> i - 1

let memSearchC = memoize searchC

seq{
    for i in 0..n-3 do
        for j in i+1..n-2 do
            yield sorted.[i], sorted.[j], j
}
|> Seq.map (fun (a,b,i) -> (memSearchC (a+b), i))
|> Seq.map (fun (x, y) -> x - y)
|> Seq.sum
|> stdout.WriteLine