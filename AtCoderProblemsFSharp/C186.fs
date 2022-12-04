module C186

let n = stdin.ReadLine() |> int64

let doesConclude7 (num :int64) =
    let with10Decimal (num :int64) :bool = 
        num.ToString().ToCharArray()
        |> Array.contains '7'

    let with8byte (num :int64) :bool =
        let rec with8byte (next :int64) =
            match next with
            | 0L -> false
            | v when v % 8L = 7L -> true
            | v -> with8byte (v / 8L)
        with8byte num

    not (with10Decimal num || with8byte num) 

[ 1L..n ]
|> Seq.filter (doesConclude7)
|> Seq.length
|> stdout.WriteLine