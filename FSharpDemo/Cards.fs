module Cards

open System

type Suit = Hearts | Clubs | Diamonds | Spades with
    override this.ToString() =
        match this with
        | Hearts -> "♥"
        | Clubs -> "♣"
        | Diamonds -> "♦"
        | Spades -> "♠"
            
type Face = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace with
    override this.ToString () =
        match this with
        | One -> "1"
        | Two -> "2"
        | Three -> "3"
        | Four -> "4"
        | Five -> "5"
        | Six -> "6"
        | Seven -> "7"
        | Eight -> "8"
        | Nine -> "9"
        | Ten -> "T"
        | Jack -> "J"
        | Queen -> "Q"
        | King -> "K"
        | Ace -> "A"

type Card = { suit: Suit; face: Face } with 
    override this.ToString () = (string this.suit) + (string this.face)
    
type Deck = Card list
type Hand = Card list

let shuffle : Deck -> Deck = List.sortBy (fun _ -> Guid.NewGuid())

let buildDeck () : Deck =
    [for suit in [Hearts; Clubs; Diamonds; Spades] do
        for face in [One; Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace] do
            yield { suit = suit; face = face }]
    
let deal (cards : Card seq) : Hand * Hand * Hand * Hand =
    let partitionBy pred =
        Seq.groupBy pred >> Seq.map snd
        
    let partitionByIndex pred =
        Seq.mapi (fun i c -> i, c) >> partitionBy (fun (i, _) -> pred i) >> Seq.map (Seq.map snd)
        
    let hands =
        cards
        |> partitionByIndex (fun i -> i % 4)
        |> Seq.map Seq.toList
        |> List.ofSeq
        
    match hands with
    | [hand1; hand2; hand3; hand4] -> hand1, hand2, hand3, hand4
    | _ -> failwith "Not got 4 hands!"
     
let printCards cards = cards |> Seq.map string |> String.concat ", "
let map4 f (a, b, c, d) = f a, f b, f c, f d
let printHands hands = map4 printCards hands

    
let deck = buildDeck() |> shuffle
deck |> deal |> printHands |> printfn "%A"