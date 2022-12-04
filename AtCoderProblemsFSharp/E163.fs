module E163

// WA
/// バブルソート、できなくなったらそこが答え、多分局所最適しないはず
let n = stdin.ReadLine() |> int
let a = stdin.ReadLine().Split() |> Array.map int64

let noA = a |> Array.mapi (fun i x -> (i, x))

let mightExchange i1 =
    let i2 = i1 + 1
    let no1, element1 = noA.[i1]
    
    if no1 <= i1 then element1
    else - element1
    |> (+) (
        let no2, element2 = noA.[i2]
        if no2 >= i2 then element2
        else - element2 )
    |> fun x -> 
        if x > 0L then
            let tmp = noA.[i1]
            noA.[i1] <- noA.[i2]
            noA.[i2] <- tmp
            true
        else
            false

let rec recExchange () =
    let doExchange =
        [0..n-2]
        |> List.map mightExchange
    if List.forall (fun x -> x = false) doExchange then ()
    else recExchange ()

recExchange ()

noA |>Array.mapi (fun i (orgi, elem) -> (i, orgi, elem)) 
|> Array.sumBy (fun (i, orgi, elem) -> int64 (abs (i - orgi)) * elem)
|> stdout.WriteLine


