module FSharpDemo.LinkedList

open System
open System.Linq
open System.Collections.Generic

[<CustomEquality; NoComparison>]
type Node<'t when 't :> obj> = {
    Guid : System.Guid
    Data : 't
    mutable Tail : LinkedList<'t>
} with
    override this.Equals other =
        match other with
        | null -> false
        | :? Node<'t> as other -> this.Guid = other.Guid
        | _ -> false
    override this.GetHashCode() = hash this.Guid

and LinkedList<'t when 't :> obj> =
    | Empty
    | Node of Node<'t>

    member this.toSeq () =
        seq {
            match this with
            | Empty -> ()
            | Node { Data = data; Tail = remaining } ->
                yield data
                yield! remaining.toSeq ()
        }

    interface seq<'t> with
        member this.GetEnumerator() =
            this.toSeq().GetEnumerator()
        member this.GetEnumerator() =
            this.toSeq().GetEnumerator() :> System.Collections.IEnumerator



let push item list =
    Node { Guid = System.Guid.NewGuid(); Data = item; Tail = list }

let containsCycle (sequence : #seq<_>) =
    use slowPointer = sequence.GetEnumerator()
    use fastPointer = sequence.GetEnumerator()
    
    let mutable foundCycle = false
    while slowPointer.MoveNext() && fastPointer.MoveNext() && (not foundCycle) do
        ignore <| fastPointer.MoveNext()

        if slowPointer.Current = fastPointer.Current
        then foundCycle <- true
        
    foundCycle