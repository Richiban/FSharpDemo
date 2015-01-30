module FSharpDemo.Tests.Unit.LinkedListTests

open FSharpDemo.LinkedList

open System
open System.Linq

open Swensen.Unquote
open Xunit

[<Fact>]
let ``Create linked list and test contents`` () =
    let list = Empty |> push 4 |> push 5 |> push 6
    let expected = [6; 5; 4]
    
    test <@ list.SequenceEqual(expected) @>

[<Fact>]
let ``Floyd does not detect cycle on empty list`` () =
    let list : int LinkedList = Empty
    
    test <@ containsCycle list = false @>

[<Fact>]
let ``Floyd does not erroneously detect cycle`` () =
    let list = Empty |> push 4 |> push 5 |> push 6
    
    test <@ containsCycle list = false @>

[<Fact>]
let ``Floyd does not erroneously detect cycle in odd numbered list`` () =
    let list = Empty |> push 4 |> push 5 |> push 6 |> push 7
    
    test <@ containsCycle list = false @>

[<Fact>]
let ``Test that a list with a cycle can be created`` () =
    let firstElement = 
        Empty 
        |> push 1

    let firstFourElements = 
        firstElement
        |> push 2
        |> push 3
        |> push 4

    let completeList =
        firstFourElements
        |> push 5
        |> push 6

    let (Node e) = firstElement
    e.Tail <- firstFourElements

    //  If we have a cycle then this should work, because 
    //  we will be able to select 20 elements from a list
    //  of only 6 elements.
    //  If not this will throw an exception
    completeList |> Seq.take 20 |> Seq.toList |> ignore

[<Fact>]
let ``Floyd algorithm correctly detects cycle in list`` () =
    let firstElement = 
        Empty 
        |> push 1

    let firstFourElements = 
        firstElement
        |> push 2
        |> push 3
        |> push 4

    let completeList =
        firstFourElements
        |> push 5
        |> push 6

    let (Node e) = firstElement
    e.Tail <- firstFourElements
    test <@ containsCycle completeList @>