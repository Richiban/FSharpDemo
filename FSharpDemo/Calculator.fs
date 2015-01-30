module Calculator

open System

type Operator = int -> int -> int

type Expression =
    | Value of int
    | SubExpression of Expression * Operator * Expression
    
let operators = // Must be in decreasing precedence order
    dict[
        "+", (+)
        "-", (-)
        "*", (*)
        "/", (/)
    ]
    
let (|Int|_|) s = 
    match Int32.TryParse s with
    | false, _ -> None
    | true, i -> Some i
    
let (|ContainsOperator|_|) (s : string) =
    let leftMiddleRight (s : String) =
        seq {
            for i in 1..(s.Length - 2) do
                let c = string s.[i]
                let left = s.Substring(0, i)
                let right = s.Substring(i + 1)
                
                yield left, c, right
        }

    operators |> Seq.tryPick(fun opKv ->
        leftMiddleRight s |> Seq.tryFind (fun (left, middle, right) -> opKv.Key = middle))

let rec parseOperation =
    function
    | Int i -> Value i
    | ContainsOperator (left, opStr, right) ->
        SubExpression (parseOperation left, operators.[opStr], parseOperation right)
    | other -> 
        failwith "'%s' is not a valid expression" other

let rec execute operation = 
    match operation with
    | Value i -> i
    | SubExpression (left, operator, right)->
        operator (execute left) (execute right)