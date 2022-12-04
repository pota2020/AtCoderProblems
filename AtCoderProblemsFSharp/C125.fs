module C125

let n = stdin.ReadLine() |> int
let a = stdin.ReadLine().Split() |> Array.map int

//メモ化
let memoize fn =
    let cache = new System.Collections.Generic.Dictionary<_,_>()
    (fun x ->
        match cache.TryGetValue x with
        | true, v -> v
        | false, _ -> 
            let v = fn (x)
            cache.Add(x,v)
            v)

// 最大公約数
let rec gcd = 
    let f (x,y) =
        let (x,y) = if y > x then (y,x) else (x,y)
        match x % y with
        | 0 -> y
        | z -> gcd (y, z)
    memoize f

// 左から順に最大公約数を求める
let rec memFromLeftGcd =
    let f i =
        match i with
        | 0 -> a.[0]
        | _ -> gcd ((memFromLeftGcd (i-1)), a.[i])
    memoize f

// 右から順に最大公約数を求める
let rec memFromRightGcd =
    let f i =
        match n - 1 - i with
        | 0 -> a.[n-1]
        | _ -> gcd ((memFromRightGcd (i+1)), a.[i])
    memoize f

seq{0..n-1}
|> Seq.map(fun i ->
    match i with
    | 0 -> memFromRightGcd <| i+1
    | i when i = n-1 -> memFromLeftGcd <| i-1
    | _ -> gcd ((memFromLeftGcd <| i-1),(memFromRightGcd <| i+1))
    )
|> Seq.max
|> stdout.WriteLine
