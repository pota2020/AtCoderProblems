module C106

let s = stdin.ReadLine()
let k = stdin.ReadLine() |> int64
if k > 100L then
    s.ToCharArray() |> Seq.find(fun x  -> x<>'1') |> stdout.WriteLine
else
    s.ToCharArray().[0 .. if int k - 1 <= s.Length - 1 then int k - 1 else s.Length - 1] |> Seq.tryFind (fun x  -> x<>'1') 
    |> function Some(v) -> v | None -> '1' 
    |> stdout.WriteLine