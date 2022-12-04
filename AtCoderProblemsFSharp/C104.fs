module C104

// TLE
open System.Collections.Generic
/// 問題数、目標得点
let [| n; dstGrade |] = stdin.ReadLine().Split() |> Array.map int
/// i*100点 問題 count個、bonus点
let countBonusPairs = [| for _ in 1..n -> stdin.ReadLine().Split() |> fun [|a1;a2|] -> (int a1, int a2) |]

// メモ化関数内にDictionary作るとなぜか毎回初期化が走るのでDictionary外出し（たぶん作り方が良くない）
let cache = Dictionary<_,_>()
let memoize f = 
    fun x ->
        if cache.ContainsKey x then cache.[x]
        else 
        cache.[x] <- f x
        cache.[x]
// ナップサック問題

/// メモ化したポイント
let rec memPoint count index problemsCount : int=

    if index = -1 then 0 // 何も使えない場合
    elif count = 0 then 0 // 何も解かない場合
    else
    /// ポイント
    let point (count, index, (problemsCount:int)) = 
        /// 一インデックス前最適解
        let accumeMinus index problemsCount = 
            if (index,problemsCount) =(0,1) then (-1,0) // point count -1 0 = 0 
            elif problemsCount = 1 then (index - 1, countBonusPairs.[index-1] |> fst)
            else (index, problemsCount - 1)

        /// 新しいインデックスを取り入れた最適解
        let pointContainNewProblem =
            /// 新しいインデックス列で用いることのできる問題数
            let usedCount = min count problemsCount
            ///新しいインデックス列の得点
            let pointNewProblem =
                if usedCount = fst countBonusPairs.[index] then 
                    (index + 1) * problemsCount * 100 + (countBonusPairs.[index] |> snd)
                else (index + 1) * min usedCount problemsCount * 100 
            /// 一列前までの最適解
            let pointOldProblem =
                if index = 0 then 0
                else  memPoint (count-usedCount) (index-1) (countBonusPairs.[index-1] |> fst)
            pointNewProblem + pointOldProblem
        
        max
            (accumeMinus index problemsCount |> fun (i,pc) -> memPoint count i pc) // 以前のインデックスまでを用いた最適解
            pointContainNewProblem

    let memPoint = memoize point
    memPoint (count,index,problemsCount)

/// 最短問題数
let dstCount =
    let rec dstCount i =
        let point = memPoint i (n-1) ( countBonusPairs.[n-1] |>fst)
        if point >= dstGrade then i
        else dstCount (i+1)
    dstCount 0

dstCount
|> stdout.WriteLine 
