module D161

let k = stdin.ReadLine() |> int

/// return newList, count, newCount
let rec createMightAnsList beforeList count =
    let newList =
        beforeList
        |> List.map (fun (a:string) -> 
            match int (string a.[a.Length-1]) with
            | 0 -> [a+"0"; a+"1"]
            | 9 -> [a+"8"; a+"9"]
            | n -> [a + string (n - 1) ; a + string n; a + string (n + 1)])
        |> List.collect id

    let newCount = count + newList.Length
    
    if newCount >= k then
        newList, count, newCount
    else
        createMightAnsList newList newCount

let countAns (newList:list<_>) count newCount = newList.[(k - count - 1)]

if k <= 9 then
    k.ToString()
else
    createMightAnsList 
        ([1..9] |> List.map string)
        9
    |||> countAns 
|> stdout.WriteLine