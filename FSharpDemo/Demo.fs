module Demo

open System

let opt = 
    function 
    | true, value -> Some value
    | _ -> None

let matchNum i =
    match i % 3, i % 5 with
    | 0, 0 -> "fizzbuzz"
    | 0, _ -> "fizz"
    | _, 0 -> "buzz"
    | _ -> "" 

let a = 
    let fizzbuzz =
        Seq.map string

    ()

System.Double.TryParse "0"
|> opt
|> printfn "%A"

(*

*)