module C177

let N = stdin.ReadLine() |> int64
let A = stdin.ReadLine().Split() |> Array.map int64
let P = 1_000_000_007L            

let cache: int64[] =
    Array.unfold 
        (fun (i, total) ->
            if i >= A.Length then None
            else Some((total + A.[i]) % P , (i + 1, (total + A.[i]) % P)))
        (0, 0L)

let sumAt (i: int): int64 =
    if i < 0 then 0L
    elif A.Length <= i then cache.[A.Length-1]
    else cache.[i]

{2L..N} |> Seq.fold (fun acc i -> ((acc + A.[int i - 1] * sumAt(int i - 2) % P)) % P) 0L |> stdout.WriteLine
