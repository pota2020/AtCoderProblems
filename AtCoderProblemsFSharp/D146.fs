module D146

// WA
open System.IO
open System

let n = stdin.ReadLine() |> int
let sides = [| for _ in 1..n-1 -> stdin.ReadLine().Split() |> Array.map int |]

// 色の数は最も辺が集約している頂点における辺の数
let variation =
    (
        (sides |> Seq.map (fun [|a;_|] -> a)) 
        , (sides |> Seq.map (fun [|_; b |] -> b))
    )
    ||> Seq.append
    |> Seq.countBy (fun x -> x)
    |> Seq.maxBy (fun x -> snd x )
    |> snd

// OKSetを作ってOKSetにある番号のうち一番若いものを組み込むことで色付けを行う
// OKSetは頂点ごとに存在する
let orgOKMap = 
    Map<int,Set<int>>
        [ for i in 1..n do
            yield (i, Set [1..variation])]

let folder ((coloredList:List<_>), (okMap:Map<_,_>)) (side:int[]) =
    let color =
        ((okMap.[side.[0]] |> Set.minElement)
        ,(okMap.[side.[1]] |> Set.minElement))
        ||> max
    let okMap =
        okMap
        |> Map.add (side.[0]) (okMap.[side.[0]].Remove(color))
        |> Map.add (side.[1]) (okMap.[side.[1]].Remove(color))
    let coloredList = color::coloredList

    (coloredList, okMap)
    
let coloredList =
    sides |> Seq.fold (folder) (List.empty,orgOKMap)
    |> fst
    |> List.rev

let sw = new StreamWriter(Console.OpenStandardOutput())
sw.AutoFlush<-false
Console.SetOut(sw)

stdout.WriteLine(variation)
for colored in coloredList do
    stdout.WriteLine(colored)

stdout.Flush()
