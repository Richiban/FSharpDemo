module PokerTests

open Poker.Parser
open Poker.Ranker
open System.IO

open Xunit
open Swensen.Unquote

let readLines (filePath : string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let pokerHands = readLines "poker.txt"

type Winner = Player1 | Player2

[<Fact>]
let ``Load and play all poker hands`` () =
    let results =
        [
            for hand1, hand2 in pokerHands |> Seq.map parseHands ->
                if (determineRank hand1) > (determineRank hand2)
                then Player1
                else Player2]

    let player1Wins, player2Wins = List.partition ((=) Player1) results

    test <@ player1Wins |> Seq.length = 376 @>