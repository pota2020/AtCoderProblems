module D171

// WA
let n = stdin.ReadLine() |> int
let a = stdin.ReadLine().Split() |> Array.map int64
let q = stdin.ReadLine() |> int
let bc = 
    [| 
        for _ in 1..q -> stdin.ReadLine().Split()|>Array.map int64
    |]

let execQueries (a:int64[]) (bc : int64[][]) =
    // 出現頻度で管理する
    let counted = 
        let array = Array.zeroCreate 10_001
        for i in a do array.[int i] <- array.[int i] + 1L
        array
    let mutable sum = a |> Array.map int64 |> Array.sum |> int64
    seq{ 
        for bcElem in bc do
            let count = counted.[int bcElem.[0]]
            counted.[int bcElem.[0]] <- 0L
            counted.[int bcElem.[1]] <- counted.[int bcElem.[1]] + count
            sum <- sum + int64 count * int64 (bcElem.[1] - bcElem.[0])
            yield sum 
    }

execQueries a bc |> Seq.iter stdout.WriteLine
