module Poker

open System
open System.Linq

type Suit = Hearts | Clubs | Diamonds | Spades

type Number = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace

type Card = Suit * Number

let suit (card : Card) = fst card
let number (card : Card) = snd card

type Hand = Card list

type Rank = | HighCard of Number | Pair of Number | TwoPairs of Number * Number | ThreeOfAKind of Number
            | Straight of highCard: Number | Flush of suit: Suit * highCard: Number | FullHouse of three: Number * pair: Number 
            | FourOfAKind of highCard: Number | StraightFlush of highCard: Number
            
module Ranker =
            
    let private getGroupsOfSize size =
        Seq.groupBy number 
        >> Seq.choose (fun (num, cards) -> let cardsList = Array.ofSeq cards in if cardsList.Length = size then Some (num, cardsList) else None ) 
        >> Seq.toList

    let (|IsFlush|_|) hand =
        let suitGroups = hand |> Seq.groupBy suit |> Seq.toList
            
        match suitGroups with
        | [ (suit, cards) ] ->
            let highCard = cards |> Seq.map number |> Seq.max
            Some (suit, highCard)
        | _ -> None
            
    let (|IsStraight|_|) hand =
        let toInt = function | Two -> 2 | Three -> 3 | Four -> 4 | Five -> 5 | Six -> 6 | Seven -> 7  | Eight -> 8
                             | Nine -> 9 | Ten -> 10 | Jack -> 11 | Queen -> 12 | King -> 13 | Ace -> 14
                                 
        let ints = hand |> Seq.map (number >> toInt) |> Seq.sort
        let isStraight = ints |> Seq.pairwise |> Seq.forall (fun (a, b) -> a + 1 = b)
            
        if isStraight 
        then 
            let highestNumber = hand |> Seq.map number |> Seq.max
            Some highestNumber 
        else None
            
    let (|HasPair|_|) hand =
        match hand |> getGroupsOfSize 2 with
        | [(num, _)] -> Some num
        | _ -> None
            
    let (|HasTwoPairs|_|) hand =
        match hand |> getGroupsOfSize 2 with
        | [(num1, _); (num2, _)] -> Some (num1, num2)
        | _ -> None
            
    let (|HasThree|_|) hand =
        match hand |> getGroupsOfSize 3 with
        | [(num, _)] -> Some num
        | _ -> None
            
    let (|IsFourOfAKind|_|) hand =
        match hand |> getGroupsOfSize 4 with
        | [(num, _)] -> Some num
        | _ -> None
            
    let (|HighCard|) hand =
        hand |> Seq.map number |> Seq.max
           
    let determineRank (hand : Hand) =
        match hand with
        | IsFlush _ & IsStraight highCard -> StraightFlush highCard
        | IsFourOfAKind number -> FourOfAKind number
        | HasThree threeNum & HasPair pairNum -> FullHouse (threeNum, pairNum)
        | IsFlush highCard -> Flush highCard
        | IsStraight highCard -> Straight highCard
        | HasThree number -> ThreeOfAKind number
        | HasTwoPairs (number1, number2) -> TwoPairs (number1, number2)
        | HasPair number -> Pair number
        | HighCard highCard -> HighCard highCard

module Parser = 
    let parseSuit =
        function
        | 'H' -> Hearts
        | 'C' -> Clubs
        | 'D' -> Diamonds
        | 'S' -> Spades
        | c -> failwithf "'%c' doesn't appear to be a valid suit" c

    let parseNumber =
        function
        | '2' -> Two
        | '3' -> Three
        | '4' -> Four
        | '5' -> Five
        | '6' -> Six
        | '7' -> Seven
        | '8' -> Eight
        | '9' -> Nine
        | 'T' -> Ten
        | 'J' -> Jack
        | 'Q' -> Queen
        | 'K' -> King
        | 'A' -> Ace
        | c -> failwithf "'%c' doesn't appear to be a valid face Number" c

    let parseCard (s : String) =
        match s.ToCharArray() with
        | [| numberChar; suitChar |] ->
            (parseSuit suitChar), (parseNumber numberChar)
        | _ -> failwithf "%s does not appear to be a valid card" s
        
    let parseHands (s : String) =
        let rawCards = s.Split(' ')
        if rawCards.Length <> 10 then failwith "Line does not contain 10 cards!"
        
        let parsedCards = rawCards |> Seq.map parseCard 
        let playerOneHand = parsedCards |> Seq.take 5 |> Seq.toList
        let playerTwoHand = parsedCards |> Seq.skip 5 |> Seq.take 5 |> Seq.toList
        
        playerOneHand, playerTwoHand