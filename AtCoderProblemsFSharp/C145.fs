module C145

let n = stdin.ReadLine() |> int
let xys =  [| for _ in 1..n -> stdin.ReadLine().Split() |> fun [|x;y|] -> (int x, int y) |]

let allPatern = 
    let rec f i tmpList =
        if i > n-1 then
            tmpList
        else
            let newTmpList = 
                tmpList |> Seq.collect (fun x ->
                    [0..n-1] |> Seq.except x  
                    |> Seq.map (fun y -> y :: (x |> Seq.toList)))
            f (i+1) newTmpList
    f 1 [for i in 0..n-1 -> [i]] 

let distance (i1,i2) =
    pown (fst (xys.[i1]) - fst (xys.[i2])) 2 + pown (snd (xys.[i1]) - snd (xys.[i2])) 2 |> fun x -> sqrt <| float x 

allPatern |> Seq.map ( fun patern ->
    patern |> Seq.pairwise |> Seq.map distance |> Seq.sum)
|> Seq.average
|> stdout.WriteLine