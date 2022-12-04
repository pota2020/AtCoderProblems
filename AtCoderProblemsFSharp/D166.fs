module D166

let x = stdin.ReadLine() |> int64

// -1000 < A,B < 1000 なので全網羅で十分間に合うはず

let candidate = {-1000L..1000L} |> Seq.allPairs {-1000L..1000L}
candidate |> Seq.find (fun (a, b) -> pown a 5 - pown b 5 = x) |> fun (a, b) -> (string a, string b) |> fun (a, b) -> a + " " + b |> stdout.WriteLine 