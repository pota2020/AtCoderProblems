module C151

let n, m = stdin.ReadLine().Split() |> Array.map int |> fun [|x1; x2|] -> x1, x2
let pStrs =
    [| for _ in 1..m -> stdin.ReadLine().Split() |> fun [|x1; x2|] -> int x1, x2 |]

let acSet, waMap =
    pStrs
    |> Seq.fold 
        (fun (acSet,waMap) (p, str) ->
            match str with
            |"AC" ->
                ((acSet |> Set.add p)
                , waMap)
            |"WA" -> 
                (acSet
                , 
                    if acSet.Contains p then waMap
                    else waMap |> Map.add p (waMap |> Map.tryFind p |> function Some(v) -> v+1 | None  -> 1)))
        (Set.empty, Map.empty)

((acSet |> Set.count)
, waMap |> Seq.sumBy (fun kp -> if acSet.Contains kp.Key then kp.Value else 0))
|> fun x -> (fst x |> string) + " " + (snd x |> string)
|>stdout.WriteLine