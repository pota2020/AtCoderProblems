module D131

let n = stdin.ReadLine() |> int
let ab = [| for i in 1..n -> stdin.ReadLine().Split() |> Array.map int |]

let sorted = ab |> Array.sortBy(fun [| _; y; |] -> y) 
let doesSuccess =
    let rec f i xTotal =
        let xTotal = xTotal + sorted.[i].[0]
        match n - i - 1,sorted.[i].[1] - xTotal >= 0 with
        | 0, true -> "Yes"
        | _, false -> "No"
        | _, true -> f (i + 1) xTotal
    f 0 0

doesSuccess |> stdout.WriteLine 
