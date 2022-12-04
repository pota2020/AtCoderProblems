module ABC098

let n = stdin.ReadLine() |> int
let s = stdin.ReadLine().ToCharArray()

let charCount char = s |> Array.filter (fun x -> x = char)|> Array.length
let wNum, eNum = charCount 'W', charCount 'E'

let countAns =
    let ansArray = Array.zeroCreate n
    let rec countAns i =
        if 0 < i && i < n - 1 then
            let acc =
                if s.[i-1] = 'W' then 1 else 0
                - if s.[i] = 'E' then 1 else 0 
            ansArray.[i] <- ansArray.[i-1] + acc
            countAns (i+1)
        elif i = 0 then
            ansArray.[0] <- eNum - (if s.[i] = 'E' then 1 else 0)
            countAns (i+1)
        else 
            let acc =
                if s.[i-1] = 'W' then 1 else 0
                - if s.[i] = 'E' then 1 else 0 
            ansArray.[i] <- ansArray.[i-1] + acc

    countAns 0
    ansArray

countAns |> Array.min |> stdout.WriteLine 