module C180

open System

let n = stdin.ReadLine() |> int64
let max = sqrt (float n) |> int64 |> (+) 1L

new IO.StreamWriter(Console.OpenStandardOutput(), AutoFlush = false) |> Console.SetOut

let result = 
    seq{ for i in 1L..max do
         if n % i = 0L then 
            yield i 
            yield n / i }
    |> Seq.sort
    |> Seq.distinct

result
|> Seq.iter stdout.WriteLine
stdout.Flush()