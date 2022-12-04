module C144

open System

let n = stdin.ReadLine() |> int64

let halfN = float n |> Math.Sqrt |> int64
 
let rec inf i =
    seq{
        yield i
        yield! inf (i-1L)
    }

inf halfN |> Seq.find (fun x -> n % x = 0L) |> fun x -> (x, n / x)
||> (+) |> fun x -> x - 2L |> stdout.WriteLine