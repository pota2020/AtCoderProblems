module C152

// n <= 2*10^5
let n = stdin.ReadLine() |> int
let ps = stdin.ReadLine().Split() |> Array.map int

ps 
|> Seq.fold 
    (fun (min, count) x ->
        if min >= x then (x, count+1)
        else (min, count))
    (200001, 0)
|> snd
|> stdout.WriteLine