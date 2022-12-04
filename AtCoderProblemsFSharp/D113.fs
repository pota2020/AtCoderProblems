module D113

open System.Collections.Generic

let p = 1000000007L

let h, w, k = stdin.ReadLine().Split() |> fun [|x1; x2; x3|] -> int x1,int x2,int x3

let addP x y = (x + y) % p
let timeP x y = x * y % p


let memoize fn =
   let hash = new Dictionary<_,_>()
   (fun x ->
       let mightValue = hash.TryGetValue(x)
       if fst mightValue then
           snd mightValue
       else 
           let v = fn x
           hash.Add(x, v)
           v)

/// (h=1,w)時の組み合わせ総数
let c = dict [
   (-1,0L)
   (0,1L)
   (1,1L)
   (2,2L)
   (3,3L)
   (4,5L)
   (5,8L)
   (6,13L)
   (7,21L)
   (8,34L)]

/// (h=1,w,kの時の初期値)
let ansH1 w k =
   match k with
   | 1 -> c.[w-1]
   | 2 -> c.[w-2]
   | _ -> 0L

//メモ化再帰を用いる。なぜこれで内部にmemoizeが働くのか原理は不明
let rec ans =
   let recAns (h,w,k) =
       match h, k with
       | _, 0  -> 0L 
       | _, k when k > w  -> 0L
       | 1, _ -> ansH1 w k
       | _ ->
           ans ((h-1), w, k) |> timeP c.[w-k] |> timeP c.[k-1]
           |> addP ( ans ((h-1), w, (k+1)) |> timeP c.[w-k-1] |> timeP c.[k-1] )
           |> addP ( ans ((h-1), w, (k-1)) |> timeP c.[w-k] |> timeP c.[k-2] )
   memoize recAns

let memAns = memoize ans
memAns (h, w, k) 
|> stdout.WriteLine