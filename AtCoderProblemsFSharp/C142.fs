module C142

open System.Text

let n = stdin.ReadLine() |> int
let a = stdin.ReadLine().Split() |> Array.map int
a |> Array.mapi (fun i x -> i + 1, x) |> Array.sortBy (fun (_,x) -> x) 
|> Array.fold (fun (total: StringBuilder) (i, _) -> total.Append(string i).Append(" ")) (StringBuilder())
|> string
|> stdout.WriteLine
