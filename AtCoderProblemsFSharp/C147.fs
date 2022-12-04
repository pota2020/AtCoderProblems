module C147

let n = stdin.ReadLine() |> int
let (a:int[]),(xyPairs:(int*int)[][]) =
    let rec inicialize aList xyPairs i =
        if i = n then (aList|>List.rev|>List.toArray, xyPairs|>List.rev|>List.toArray)
        else
        let a = stdin.ReadLine() |> int
        let aList = a :: aList
        let xyEachPairs = [| for _ in 1..a -> stdin.ReadLine().Split() |> fun [|x1;x2|]->(int x1 - 1, int x2) |]
        let xyPairs = xyEachPairs :: xyPairs
        inicialize aList xyPairs (i+1)
    inicialize [] [] 0 

// Nが小さいので2^Nパターン列挙して最大値を求める
let allPatern = 
    [((1<<<n)-1).. -1 .. 0] 
    |> Seq.map (fun k -> 
        [| let mutable k = k
        for i in 1..n do
            yield k % 2
            k <- k / 2 |])
    |> Seq.sortByDescending (fun array -> array |> Array.filter (fun x -> x = 1) |> Array.length)

let detect (patern:int[]) =
    let getIResult i x =
        if x = 0 then true
        else 
            xyPairs.[i]
            |> Seq.map (fun (x,y) -> patern.[x] = y)
            |> Seq.forall (fun x -> x = true)
        
    patern
    |> Seq.mapi (getIResult)
    |> Seq.forall (fun x -> x )

allPatern |> Seq.find detect |> Array.filter (fun x -> x = 1) |> Array.length |> stdout.WriteLine