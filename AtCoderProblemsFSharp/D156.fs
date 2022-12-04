module D156

open System

module Util =
    
    /// 正の整数の2bit分解(逆順)
    let _2bitSeq x :seq<bool> =
        Seq.unfold (fun before ->
            if before = 0L then 
                None
            else
                Some(before % 2L |> Convert.ToBoolean , before / 2L) ) 
            x


    /// x^y(mod m)
    let power x y m =
        _2bitSeq y
        |> Seq.fold (fun (p, total) bit ->
            if bit then
                (p * p % m, total * p % m)
            else
                (p * p % m, total))
            (x, 1L)
        |> snd


    /// x^(-1) (mod m) (x,mは互いに素)
    let rev x m =
        // フェルマーの小定理を用いる
        power x (m-2L) m


    /// xCy(mod m)
    let combination x y m =
        let nomalSeq = 
            seq{ x .. -1L .. (x - y + 1L)}
        
        let revSeq =
            seq{1L .. y}
            |> Seq.map (fun x -> rev x m)
        
        let nomalTotal =
            nomalSeq
            |> Seq.fold (fun total x -> total * x % m) 1L

        let revTotal =
            revSeq
            |> Seq.fold (fun total x -> total * x % m) 1L

        nomalTotal * revTotal % m


    /// x - y (mod m)
    let minus x y m =
        let mutable ans = (x - y) % m
        while ans < 0L do
            ans <- ans + m
        ans


module Program =

    let main =
        let p = 1000000007L
        
        let n, a, b =
            stdin.ReadLine().Split() |> Array.map int64 |>fun array -> array.[0], array.[1], array.[2]
        
        Util.power 2L n p - 1L
        |> fun x -> Util.minus x (Util.combination n a p) p
        |> fun x -> Util.minus x (Util.combination n b p) p
        |> stdout.WriteLine
