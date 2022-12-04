module C157

let n, m = stdin.ReadLine().Split() |> fun array -> int array.[0], int array.[1]
let scPairs = [|
    for _ in 1.. m do
        yield stdin.ReadLine().Split() |> fun array -> int array.[0], int array.[1]
|]

let predicateDetail num (s,c) = 
    (string num).ToCharArray() 
    |> fun array ->
        if s-1 >= array.Length then false
        elif array.[s-1] <> char (string c) then false
        else true

let predicate num =
    scPairs 
    |> Seq.forall (predicateDetail num) 

match n with
| 1 -> [0..9]
| _ ->
    [pown 10 (n-1)..(pown 10 n) - 1]
|> Seq.tryFind (fun num -> predicate num)
|> function
| Some(v) -> v
| None -> -1
|> stdout.WriteLine
