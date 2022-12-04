module C171

let n = stdin.ReadLine() |> int64

let str26 n :int64[]= 

    let unfolder tmp =
        if tmp = 0L then None
        else
        let tmp = tmp - 1L
        let out = tmp % 26L
        let tmp = (tmp - out) / 26L
        Some(out, tmp)
        
    Array.unfold unfolder n
    |> Array.rev

str26 n |> Array.map (fun x -> x + 97L |> char) |>fun x -> new string(x) |> stdout.WriteLine