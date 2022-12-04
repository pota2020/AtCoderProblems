module C143

let n = stdin.ReadLine() |> int
let s = stdin.ReadLine()

s.ToCharArray() 
|> Seq.fold (fun (count, beforeC) c ->
    match c = beforeC with
    | true -> count, c
    | false -> count+1, c) 
    (0,'#')
|> fun (count, _) -> stdout.WriteLine(count)