module C162

let k = stdin.ReadLine() |> int

let gcd2 x y=
    let x1, x2=
        if y <= x then y,x
        else x, y
    let rec gcdrec2 x1 x2 =
        if x1%x2 = 0 then x2
        else gcdrec2 x2 (x1%x2)
    gcdrec2 x1 x2

let gcd3 x y z =
    let x2 =gcd2 x y
    if x2 >= z then gcd2 x2 z
    else gcd2 z x2

let ans =
    let mutable ans = 0L
    for i1 in {1..k} do
        for i2 in {1..k} do
for i3 in {1..k} do
    ans <- ans + int64 (gcd3 i1 i2 i3)
    ans

string ans |> stdout.WriteLine
