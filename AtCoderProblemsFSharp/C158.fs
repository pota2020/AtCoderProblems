module C158

open System

let a, b = stdin.ReadLine().Split() |>fun list -> (list.[0], list.[1])

[ 
    (float a * 100.0 / 8.0 |> Math.Ceiling |> int)
        ..
        (float ( int a + 1 ) * 100.0 / 8.0 |> Math.Ceiling |> int) - 1 
]
|> Set.ofList
|> Set.intersect (
    [ 
        ( float b * 100.0 / 10.0 |> Math.Ceiling |> int )
            ..
            ( float ( int b + 1 ) * 100.0 / 10.0 |> Math.Ceiling |> int ) - 1
    ]
    |> Set.ofSeq
)
|> function
| set when set.IsEmpty -> -1
| set -> set |> Set.minElement
|> stdout.WriteLine
