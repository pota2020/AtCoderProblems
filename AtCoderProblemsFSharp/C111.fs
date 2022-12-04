module C111

let n = stdin.ReadLine() |> int
let values = stdin.ReadLine().Split() 

// 出現回数マックスの文字と出現回数
let maxAppearNum seq = seq |> Seq.countBy (fun x -> x) |> Seq.maxBy (fun (str,count) -> count) 

let nextMaxAppearNum seq maxStr =
    seq |> Seq.filter (fun x -> x <> maxStr) |> Seq.toList 
    |> function
    |[] -> ("#",0)
    |x -> maxAppearNum x


let evenValues, oddValues =
    values |> Seq.mapi (fun i x -> i,x) |> Seq.filter (fun (i,_) -> i%2 = 0) |> Seq.map snd
    ,values |> Seq.mapi (fun i x -> i,x) |> Seq.filter (fun (i,_) -> i%2 = 1) |> Seq.map snd

(
    maxAppearNum evenValues,
    maxAppearNum oddValues
)
||> fun (str1,count1) (str2,count2) -> 
    if str1 = str2 then
        min
            (
                let (str2,count2) = nextMaxAppearNum oddValues str2 
                n - count1 - count2
            )
            (
                let (str1,count1) = nextMaxAppearNum evenValues str1
                n - count1 - count2
            )

    else
        n - count1 - count2
|> stdout.WriteLine