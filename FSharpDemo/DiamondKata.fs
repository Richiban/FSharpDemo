module DiamondKata

open System

let (|Alphabet|) c =
    if c < 'A'|| c > 'Z'
    then raise (new ArgumentException("Input must be in A..Z"))
    else c

let numSpaces c =
    let chars = ['A' .. 'Z']
    let nums = [0 .. 25]
    
    (Seq.zip chars nums |> dict).[c]

let make (Alphabet(c)) =
    let lines =
        let getLine c =
            if c = 'A'
            then "A"
            else (string c) + (String.replicate (numSpaces c) " ") + (string c)

        let prevChar = (int c) - 1 |> char
        let upChars = { 'A'..prevChar }
        let downChars = upChars |> Seq.toList |> List.rev

        seq {
            for char in upChars -> getLine char
            yield getLine c
            for char in downChars -> getLine char
        }

    String.concat Environment.NewLine lines