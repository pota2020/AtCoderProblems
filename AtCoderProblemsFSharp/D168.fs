module D168

open System.Collections.Generic
open System.Linq

let n, m = stdin.ReadLine().Split()|> fun [| n; m; |] -> int n, int m
let abArray :(int*int)[] = 
    [|1..m|] |> Array.map (fun _ -> stdin.ReadLine().Split() |> fun [| a; b; |] -> int a, int b)

/// 計算のため逆方向の道も作成
let abArray2 abArray = abArray |> Array.collect (fun (a, b) -> [| (a, b); (b, a) |] )

/// 部屋番号とそれに対応する道しるべを持つmapを作成
let makeDirectionMap (abArray2 :(int*int)[]) =
    let abLookup = abArray2.ToLookup(fun (a, b) -> a)
    let goneNo = new HashSet<int>([1])
    let notGoneNo = new HashSet<int>({2..n})
    let tmpDirectionMap = new Dictionary<int,int>()

    let rec makeDirectionMap (nos:Set<int>) =
        let nosPairs :(int*int)[] = 
            nos|> Seq.collect (fun no -> abLookup.[no]) |> Seq.filter (fun (_ , x) -> notGoneNo.Contains(x) ) |>Seq.toArray
        nosPairs |> Seq.iter (fun (x1, x2) -> 
            goneNo.Add x2 |> ignore
            notGoneNo.Remove x2 |> ignore
            tmpDirectionMap.TryAdd(x2,x1)|>ignore)
        if not (nosPairs.Any()) then tmpDirectionMap
        else makeDirectionMap (nosPairs |> Seq.map snd |> set) 

    makeDirectionMap (set [1])

let directionMap = abArray |> abArray2 |> makeDirectionMap
stdout.WriteLine "Yes"
[2..n] |> List.iter (fun i -> stdout.WriteLine(string directionMap.[i]))
