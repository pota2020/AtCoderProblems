module C101

let [| n; k |] = stdin.ReadLine().Split() |> Array.map int
let a = stdin.ReadLine().Split() |> Array.map int

ceil (float (n-1) / float (k-1)) |> int |> stdout.WriteLine
