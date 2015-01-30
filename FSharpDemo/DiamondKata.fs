module DiamondKata

open System

let (|Alphabet|) c =
    if c < 'A'|| c > 'Z'
    then raise (new ArgumentException("Input must be in A..Z"))
    else c
    
let numberedChars = 
    Seq.zip {'A' .. 'Z'} {1 .. 26} |> dict

let toNum c = numberedChars.[c]

let numSpaces c = ((toNum c) - 1) * 2 - 1

let make (Alphabet(c)) =
    let numLines = (toNum c) * 2 - 1
    let lines =
        let getLine c =
            let expectedLength = numLines
            //failwithf "Expected length: %A" expectedLength
            let innerLine =
                if c = 'A'
                then "A"
                else (string c) + (String.replicate (numSpaces c) " ") + (string c)

            let numPadding = (expectedLength - innerLine.Length) / 2
            let padding = (String.replicate numPadding " ")

            padding + innerLine + padding

        let prevChar = (int c) - 1 |> char
        let upChars = { 'A'..prevChar }
        let downChars = upChars |> Seq.toList |> List.rev

        seq {
            for char in upChars -> getLine char
            yield getLine c
            for char in downChars -> getLine char
        }

    String.concat Environment.NewLine lines