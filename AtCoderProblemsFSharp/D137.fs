module D137

let [| n; m|] = stdin.ReadLine().Split() |> Array.map int
let abArray = [| for _ in 1..n -> stdin.ReadLine().Split() |> Array.map int64 |]

let abArraySorted = Array.create<int64[][]> (m+1) [|[|0L;0L|]|]
abArray |> Array.groupBy (fun [| a; b; |] -> a) |> Array.iter (fun (a, array) -> if int a <= m then abArraySorted.[int a] <- array ) 

let appendSet i set =
    abArraySorted.[i] |> Seq.mapi (fun i [| a; b; |] -> b, a, i) |> Set.ofSeq |> Set.union set

// 一回のバイト
let job (set, i) = 
    match i with
    | -1 -> None
    | _ ->
        let set = appendSet (m-i) set
        let max = set |> Set.maxElement 
        let setNext = set |> Set.remove max
        Some( (max|> fun (b, a, i) -> b), (setNext, (i-1)))

Seq.unfold job (Set.empty,m-1)
|>Seq.sum
|>stdout.WriteLine
