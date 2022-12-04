module C154

let n = stdin.ReadLine() |> int
let aArray = stdin.ReadLine().Split() |> Array.map int

let aDist = aArray |> Array.distinct

if aDist.Length = aArray.Length then
    "YES"
else
    "NO"
|> stdout.WriteLine