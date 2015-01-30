module Hangman

type State = char option list

let wordMatches state word =
    if List.length state <> String.length word
    then false
    else
        Seq.zip state word
        |>  Seq.forall (
            function
            | None, _ -> true
            | Some c1, c2 -> c1 = c2
        )

let findMatches dictionary state =
    dictionary |> List.filter (wordMatches state)

let dictionary = 
    List.sort [
        "cool"
        "above"
        "photography"
        "chips"
        "chill"
        "chimp"
        "camber"
        "camambert"
    ]

let suggestLetter state =
    match findMatches dictionary state with
    | [] -> failwith (sprintf "There aren't any possible words! State = %A" state)
    | matches ->
        matches
        |> Seq.tryPick (fun suggestedWord ->
            Seq.zip state suggestedWord
            |> Seq.tryPick (
                function
                | None, c -> Some c
                | Some _, _ -> None )
        )

let state = [Some 'c'; None; Some 'i'; None; None]

type PlayerTurnResult = CorrectGuess of State | FailedGuess of State

let playerTurn (answer : string) state guessedLetter =
    if answer.Contains (guessedLetter.ToString())
    then
        Seq.zip state answer
        |> Seq.map (
            function
            | None, c when c = guessedLetter -> Some c
            | stateChar, _ -> stateChar )
        |> List.ofSeq
        |> CorrectGuess
    else FailedGuess state

let (|GameFinished|Unfinished|) state =
    if state |> Seq.forall ((<>)None) then GameFinished else Unfinished

let rec gameLoop answer state =
    async {
        let suggestedLetter = suggestLetter state
        printfn "Suggested letter: %A" suggestedLetter

        match suggestedLetter with
        | None -> failwith "Couldn't suggest a letter; I'm stuck!"
        | Some c ->
            let turnResult = playerTurn answer state c
            printfn "Turn result: %A" turnResult

            match turnResult with
            | CorrectGuess newState ->
                return! gameLoop answer newState
            | FailedGuess state ->
                return! gameLoop answer state
    }

let initialiseState answer =
    List.replicate (String.length answer) None