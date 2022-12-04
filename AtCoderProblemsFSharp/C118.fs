module C118

let n = stdin.ReadLine() |> int
let hp = stdin.ReadLine().Split() |> Array.map int

// 最大公約数
let rec gcd x y = 
    let (x,y) = if y > x then (y,x) else (x,y)
    match x % y with
    | 0 -> y
    | z -> gcd y z

hp |> Seq.reduce (fun acc x -> gcd acc x) |> stdout.WriteLine