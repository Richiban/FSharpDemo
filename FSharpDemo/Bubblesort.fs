module Bubblesort

open System
    
let rec bubblesortPass list =
    match list with
    | [] | [_] -> list
    | one :: two :: tail ->
        if one > two
        then two :: (bubblesortPass (one :: tail))
        else one :: (bubblesortPass (two :: tail))

let rec bubblesort =
    function
    | [] | [_] as list -> list
    | list ->
        let passResult = bubblesortPass list

        let revResult = passResult |> List.rev

        match revResult with
        | [] -> failwith "Ah!"
        | last :: revTail ->
            (bubblesort (revTail |> List.rev)) @ [last]

let input = [4; 790; 234; 98; 89]

bubblesort input |> printfn "%A"