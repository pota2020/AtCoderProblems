module C185

let n = stdin.ReadLine() |> int64

// nC11をオーバーフローしないように求める
let result = 
    ([n-1L .. -1L .. (n-11L)], [1L..11L])
    ||> List.zip
    |> List.fold 
        (fun acc (timed, divided) -> acc * timed / divided)
        (1L)

stdout.WriteLine(result)