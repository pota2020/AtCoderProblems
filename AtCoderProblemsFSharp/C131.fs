module C131

let [| a; b; c; d|] = stdin.ReadLine().Split()|>Array.map int64

// 最大公約数
let rec gcd x y = 
    let (x,y) = if y > x then (y,x) else (x,y)
    match x,y with
    | _ when x % y = 0L -> y
    | _ -> gcd y (x % y)

// 最小公倍数
let lcm x y = x / gcd x y * y

// B以下の割り切れない個数
let ans downX =
    downX - (downX / c + downX / d - downX / lcm c d)

ans b - ans (a-1L) |> stdout.WriteLine