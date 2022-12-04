module C170

let x, n = stdin.ReadLine().Split()|> fun [| a; b |] -> int a, int b
let p = 
    stdin.ReadLine()|> function
    | "" -> Array.empty
    | x -> x.Split() |> Array.map int

// 制約がきついので総当たりで調べても間に合う
let cand x =
    let rec createCand diff =
        seq{
            if diff > 100 then ()
            elif diff = 0 then
                yield x
                yield! createCand (diff+1)
            else
                yield x - diff
                yield x + diff
                yield! createCand (diff + 1)
        }
    createCand 0 

if n = 0 then stdout.WriteLine x 
else
cand x |> Seq.find (fun x -> not (Array.contains x p)) |> stdout.WriteLine 