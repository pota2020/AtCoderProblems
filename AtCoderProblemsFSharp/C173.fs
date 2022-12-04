module C173

let [| H; W; K; |] = stdin.ReadLine().Split() |> Array.map int
let CArrExtended =
    [| for _ in 1..H -> stdin.ReadLine().ToCharArray() |]

let blackCount (num: int): option<int> =
    let bitArray num : bool[] = 
        [| 
            let mutable tmp = num
            for _ in 0..11 do 
                yield tmp % 2 = 1
                tmp <- tmp / 2
        |]
    
    let blackCount (bitArray: bool[]): option<int>=
        if bitArray.[H..5] |> Array.contains true then None
        elif bitArray.[6+W..] |> Array.contains true then None
        else
        let bitArrayRow = bitArray.[0..H-1]
        let bitArrayCollumn = bitArray.[6..5+W]
        [|0..5|] |> Array.allPairs [|0..5|]
        |> Array.filter (fun (y, x) ->           
            CArrExtended |> Array.tryItem y |> Option.bind (Array.tryItem x) 
            |> Option.map (fun x -> x = '#') 
            |> Option.defaultValue false            
                && not (bitArrayRow |> Array.tryItem y |> Option.defaultValue false )  
                && not (bitArrayCollumn |> Array.tryItem x |> Option.defaultValue false ))
        |> Array.length |> Some
    
    num |> bitArray |> blackCount

[| 0..((pown 2 12) - 1) |] |>
Array.map blackCount
|> Array.filter (fun x -> x |> Option.map (fun x -> x = K) |> Option.defaultValue false)
|> Array.length
|> stdout.WriteLine