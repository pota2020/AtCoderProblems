module D152

// WA
// n <= 2 * 10^5
let strN = stdin.ReadLine()
let n = strN |> int64

let answer =

    let rec answer k total =

        if k = n+1L then total  
        else
        let count k =
            let strK = k.ToString()
            let kHead = strK.Substring(0,1) |> int64
            let kLast = strK.Substring(strK.Length-1,1) |> int64
            let nHead = strN.Substring(0,1) |> int64
            let nLast = strN.Substring(strN.Length-1,1) |> int64
            if kLast = 0L then
                0L
            elif nHead > kLast then
                let tmp = (pown 10 (strN.Length - 1) - 1) / 9 |> int64
                if kHead = kLast then tmp + 1L
                else tmp
            elif nHead < kLast then
                if strN.Length = 1 then 0L
                else
                let tmp = (pown 10 (strN.Length - 2) - 1) / 9 |> int64
                if kHead = kLast then tmp + 1L
                else tmp
            else 
                if strN.Length = 1 then 
                    if kHead = kLast && kLast <= n then 1L
                    else 0L
                elif strN.Length = 2 then
                    if kLast = kHead && kLast * 10L + kHead <= n then 2L
                    elif kLast = kHead || kLast * 10L + kHead <= n then 1L
                    else 0L
                else
                    let edge =
                        if kLast <= nLast then strN.Substring(1,strN.Length-2) |> int64 |> (+) 1L
                        else  strN.Substring(1,strN.Length-2) |> int64
                    let other =
                        if strN.Length = 1 then 0L
                        else
                        let tmp = (pown 10 (strN.Length - 2) - 1) / 9 |> int64
                        if kHead = kLast then tmp + 1L
                        else tmp
                    edge + other
        
        answer (k+1L) (total + count k)
        
    answer 1L 0L

stdout.WriteLine(answer)