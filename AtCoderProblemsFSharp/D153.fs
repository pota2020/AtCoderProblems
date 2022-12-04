module D153

// k<=10^12
let h = stdin.ReadLine() |> int64

/// h=2 -> 1, h=3 -> 1, h=2^n -> n
let digit2 h = log (float h) / log 2.0 |> int 

/// 1 + 2 + 4 + ... + 2^n
digit2 h |> fun x -> pown 2L (x + 1) - 1L |> stdout.WriteLine