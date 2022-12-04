module C108

let [|n;k|] = stdin.ReadLine().Split() |> Array.map int64

// a=b=c=0(mod k) or a = b = c = k/2(modk)以外は条件を満たさない
if k%2L <> 0L then
    pown (n/k) 3
else
    pown (n/k) 3 + pown ((n+k/2L)/k) 3
|> stdout.WriteLine
