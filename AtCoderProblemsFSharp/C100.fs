module ABC100

let n = stdin.ReadLine() |> int64
let a: int64[] = stdin.ReadLine().Split() |> Array.map int64
 
let rec count list tmp =
    let filtered = list |> List.filter (fun x -> x % 2L = 0L)
    let added = filtered |> List.length
    if added = 0 then tmp
    else
    let nextList = filtered |> List.map (fun x -> x / 2L)
    count nextList (tmp + int64 added)
 
count (a |> Array.toList) 0L |> stdout.WriteLine