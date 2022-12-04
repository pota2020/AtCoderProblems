module C122

open System.IO
open System

let [| n; q; |] = stdin.ReadLine().Split() |> Array.map int
let s = stdin.ReadLine()
let lr = [| for _ in 1..q -> stdin.ReadLine().Split() |> Array.map int |]

let sw = new StreamWriter(Console.OpenStandardOutput())
sw.AutoFlush <- false
Console.SetOut(sw)

let appearanceArrayToRight =
    let sPair = s.ToCharArray() |> Array.pairwise 
    [|
        //最初の一文字
        yield 0
        let mutable app = 0
        for pair in sPair do
            if pair = ('A', 'C') then 
                app <- app + 1
            yield app
    |]

let getAppearance _begin _end =
    appearanceArrayToRight.[_end-1] - appearanceArrayToRight.[_begin-1]

lr
|> Array.map (fun [| l; r; |] -> getAppearance l r |> string)
|> Array.iter stdout.WriteLine

stdout.Flush()


