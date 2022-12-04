module C112

let n = stdin.ReadLine() |> int
type Inp =
    { X : int
      Y : int
      H : int }

let inps = 
    [|for _ in 1..n -> 
        stdin.ReadLine().Split() |> fun [|x;y;h|] -> 
            { X = int x
              Y = int y
              H = int h } |]

// 方針：Cx,Cy総当たりで求めても何とかなるはず
let h cx cy (inp:Inp) = 
    match inp.H with
    | 0 -> None
    | _ -> Some <| abs (inp.X-cx) + abs (inp.Y-cy) + inp.H

let cxcyPairs =
    seq{ for i in 0..100 do
         for j in 0..100 -> (i, j) }

cxcyPairs 
|> Seq.map 
    (fun (cx,cy) -> inps |> Seq.map (h cx cy) |> Seq.where (fun x -> Option.isSome x) |> Seq.map (function Some(x) -> x ) |>fun hs -> (cx , cy, hs))
|> Seq.find (fun (cx,cy,hs) -> (hs |> Seq.max) = (hs |> Seq.min) )
|> fun (cx,cy,hs) -> string cx + " " + string cy + " " + string ( hs |> Seq.head)
|> stdout.WriteLine