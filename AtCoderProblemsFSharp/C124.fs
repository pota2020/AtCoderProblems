module C124

let s = stdin.ReadLine()

let paturn1 = seq{0..s.Length-1} |> Seq.mapi (fun i x -> if i%2=0 then '0' else '1') |> Seq.toArray
let paturn2 = seq{0..s.Length-1} |> Seq.mapi (fun i x -> if i%2=0 then '1' else '0') |> Seq.toArray

let compare (paturn: char[]) =
    s.ToCharArray() |> Seq.zip (paturn) |> Seq.fold (fun count (x,y) ->if x<>y then count+1 else count ) 0

min (compare paturn1) (compare paturn2) |> stdout.WriteLine
