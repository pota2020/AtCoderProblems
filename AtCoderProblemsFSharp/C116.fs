module C116

let n = stdin.ReadLine() |> int
let hs = stdin.ReadLine().Split() |> Array.map int

let countZeroChunk array = 
    array 
    |> Array.fold (fun (count,before) x -> if before<>0 && x = 0 then (count+1, x) else (count, x)) (0, 0)
    |> fst
    |> fun x -> if array |> Array.last |> (=) 0 then x-1 else x 

let highest = hs |> Array.max

let grow seq = seq |> Seq.map (fun x -> if x<>0 then x-1 else 0) |> Seq.toArray 

[1..highest] |> Seq.fold (fun (count, growed) i -> (growed |> countZeroChunk |> (+) (1 + count), grow growed) ) (0, hs)
|> fst |> stdout.WriteLine