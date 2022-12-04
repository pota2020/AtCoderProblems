module D157

open System.Collections.Generic

// TLE
let edgeOver2 (roads:Map<int,seq<int>>) num =
    let gone = HashSet<int>()
    let rec count (roads:Map<int,seq<int>>) num =
        roads
        |> Map.tryFind num
        |> function
        | None -> seq []
        | Some(v) -> v
        |> Seq.iter (fun elem ->
            if gone.Contains(elem) then
                ()
            else
                gone.Add(elem) |> ignore
                count roads elem)
    count roads num
    Set gone

let main =
    let n, m, k = stdin.ReadLine().Split() |> Array.map int |> fun array -> array.[0], array.[1], array.[2]
    let abSeq = 
        seq[ for _ in 1..m -> stdin.ReadLine().Split() |> Array.map int |> fun [|a;b|] -> a, b ]
    let cdSeq = 
        seq[ for _ in 1..k -> stdin.ReadLine().Split() |> Array.map int |> fun [|c;d|] -> c, d ]

    let roads =
        abSeq
        |> Seq.append (abSeq |> Seq.map (fun (a, b)->(b, a)))
        |> Seq.groupBy (fun (a, b) -> a)
        |> Seq.map (fun (x, array) ->x, array |> Seq.map snd)
        |> Map.ofSeq
    
    let predicate4 num = edgeOver2 roads num 

    let friend num = 
        Map.tryFind num roads 
        |> function
        |Some(v) -> Set.ofSeq v
        |None -> Set.empty

    let blocked num = 
        cdSeq
        |> Seq.append (cdSeq |> Seq.map (fun (c, d)->(d, c)))
        |> Seq.groupBy (fun (c, d) -> c)
        |> Seq.map (fun (x, array) ->x, array |> Seq.map snd)
        |> Map.ofSeq
        |> Map.tryFind num
        |> function
        |None -> Set.empty
        |Some(v) -> Set.ofSeq v

    let own num = Set.ofSeq [num]

    let out num= 
        predicate4 num
        |> fun s -> Set.difference s (friend num) 
        |> fun s -> Set.difference s (blocked num) 
        |> fun s -> Set.difference s (own num) 
        |> Set.count

    seq [1..n]
    |> Seq.map out 
    |> Seq.map string
    |> Seq.reduce (fun acc x -> acc + " " + x)
    |> stdout.WriteLine