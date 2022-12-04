module D158

open System.Linq
open System.Collections.Generic

type Query =
    | Rev
    | AddFst of str: string
    | AddLst of str: string

let s = stdin.ReadLine()
let q = stdin.ReadLine() |> int
let queries = 
    [|
        for i in 1..q -> stdin.ReadLine().Split()
    |]

let parse (query:_[]) =
    match query with
    | [|"1"|] -> Rev
    | [| "2"; "1"; x3|] -> AddFst(x3)
    | [| "2"; "2"; x3|] -> AddLst(x3)


let buildStr (queries:seq<Query>)  : string =

    let charArray = new LinkedList<char>(s.ToCharArray())
    let mutable doesRev = false 
    let build query = 
        match query with
        | Rev -> doesRev <- not doesRev
        | AddFst(str2) -> if not doesRev then charArray.AddFirst(char str2) |> ignore else charArray.AddLast(char str2) |> ignore
        | AddLst(str2) -> if doesRev then charArray.AddFirst(char str2) |> ignore else charArray.AddLast(char str2) |> ignore

    for query in queries do
        build query

    if not doesRev then new string(charArray.ToArray()) else new string(charArray.Reverse().ToArray())


queries
|> Array.map parse
|> buildStr
|> stdout.WriteLine