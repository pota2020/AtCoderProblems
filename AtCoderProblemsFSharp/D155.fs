module D155

open System

type Group =
    | Plus
    | Minus
    | Zero
let maxA = pown 10L 18
let minA = -maxA

let n, k = stdin.ReadLine().Split() |> fun array -> int64 array.[0], int64 array.[1]
let aArray = stdin.ReadLine().Split() |> Array.map int64

// 正、0、負を取る組み合わせの総数を求める
let plus, _0, minus =
    let plusTmp = (aArray |> Array.filter (fun x -> x > 0L))|> Array.length |> int64
    let minusTmp = (aArray |> Array.filter (fun x -> x < 0L))|> Array.length |> int64
        
    let plus = plusTmp * (plusTmp - 1L) / 2L + minusTmp * (minusTmp - 1L) / 2L
    let minus = plusTmp * minusTmp
    
    (
        plus
        , n * (n - 1L) / 2L - (plus + minus)
        , minus
    )

// kがどのタイプか判断する
let kType =
    if k <= minus then Minus
    elif k<= minus + _0 then Zero
    else Plus

/// 負の数の場合の二分探索
let findMinus k aArray =
    let plusSortedArray = aArray |> Array.filter (fun x -> x > 0L) |> Array.sort
    let minusSortedArray = aArray |> Array.filter (fun x -> x < 0L) |> Array.sort
    /// aArrayに対してk番目に小さい負の数をmin,max範囲内で探索する 
    let rec findMinus1 min max =
        /// cand以下の数の組み合わせ個数
        let _count cand =
            plusSortedArray
            |> Array.map (fun plus -> float cand / float plus |> floor |> int64)
            |> Array.map (fun _cand -> 
                minusSortedArray 
                |> fun array -> Array.BinarySearch(array, _cand) |> int64) 
            |>  Array.map (fun x -> abs <| x + 1L )
            |> Array.sum
        if min = max then
            min
        elif min + 1L = max then
            if _count min >= k then 
                min
            else
                max
        else
            let cand = (min + max) / 2L
            if _count cand < k then 
                findMinus1 (cand + 1L) max
            else
                findMinus1 min cand
    findMinus1 minA -1L

/// 正の数の場合の二分探査
let findPlus k aArray =
    let plusSortedArray = aArray |> Array.filter (fun x -> x > 0L) |> Array.sort
    let minusSortedArray = aArray |> Array.filter (fun x -> x < 0L) |> Array.sort
    /// aArrayに対してk番目に小さい正の数をmin,max範囲内で探索する 
    let rec findPlus1 min max =
        let countPlus cand array =
            array
            |> Array.mapi (fun i x ->
                let a = cand / x
                if i = 0 then
                    -1L
                else
                array.[.. i - 1] 
                |> fun _array -> Array.BinarySearch(_array,a) |> int64) 
            |> Array.map (fun x -> x + 1L |> abs)
            |> Array.sum
        
        let countMinus cand array =
            let array = array |> Array.map abs |> Array.rev
            countPlus cand array

        if min = max then
            min
        elif min + 1L = max then 
            if countPlus min plusSortedArray + countMinus min minusSortedArray >= k then
                min
            else
                max
        else
            let cand = (min + max) / 2L
            if countPlus cand plusSortedArray + countMinus cand minusSortedArray < k then
                findPlus1 (cand + 1L) max
            else
                findPlus1 min cand
    findPlus1 1L maxA 

match kType with
| Minus ->
    findMinus k aArray
| Plus -> 
    let k = k - minus - _0
    findPlus k aArray
| Zero ->
    0L
|> stdout.WriteLine