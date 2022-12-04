module C110

// WA
open System.Collections.Generic

let org = stdin.ReadLine()
let dst = stdin.ReadLine()

// すべての単語に対して出現回数が両者一致していることが必要条件

let appearanceCountmap (str:string) =
   let map = Dictionary<char,int> ()
   str.ToCharArray() |> Seq.map (fun char -> 
       if not (map.ContainsKey char) then 
           map.[char] <- 1
           1
       else
           map.[char] <- map.[char]+1
           map.[char])

if appearanceCountmap org = appearanceCountmap dst then "Yes"
else "No"
|> stdout.WriteLine