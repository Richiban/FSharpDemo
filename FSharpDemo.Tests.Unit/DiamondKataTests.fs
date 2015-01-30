module DiamondKataTests

open System

open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote

type Letters =
    static member Char() =
        Arb.Default.Char()
        |> Arb.filter (fun c -> 'A' <= c && c <= 'Z')

type DiamondPropertyAttribute() =
    inherit PropertyAttribute(
        Arbitrary = [| typeof<Letters> |],
        QuietOnSuccess = true)

let toLines (s : string) = s.Split ([|Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries)
let uncurry f (x, y) = f x y

let toNum c =
    let chars = ['A' .. 'Z']
    let nums = [1 .. 26]
    
    (Seq.zip chars nums |> dict).[c]

[<Fact>]
let ``Test character A``() =
    let expected = "A"
    let actual = DiamondKata.make 'A'
    test <@ actual = expected @>

[<Fact>]
let ``Test character B``() =
    let expected = " A " + Environment.NewLine + "B B" + Environment.NewLine + " A "
    let actual = DiamondKata.make 'B'
    test <@ actual = expected @>
        
[<DiamondProperty>]
let ``Diamond is non-empty`` (letter : char) =
    let actual = DiamondKata.make letter

    test <@ not (String.IsNullOrWhiteSpace actual) @>

[<DiamondProperty>]
let ``Diamond is vertically symmetrical`` (letter : char) =
    let actual = DiamondKata.make letter

    let linesBeforeReflection = toLines actual
    let linesAfterReflection = linesBeforeReflection |> Array.rev

    test <@ linesBeforeReflection = linesAfterReflection @>

[<DiamondProperty>]
let ``Diamond output is square`` (letter : char) =
    let actual = DiamondKata.make letter

    let lines = toLines actual

    //  The length of every line is equal to the number of lines
    test <@ lines |> Seq.forall (fun line -> line.Length = lines.Length) @>

[<DiamondProperty>]
let ``Diamond output has no two consecutive lines the same`` (letter : char) =
    let actual = DiamondKata.make letter

    let lines = toLines actual
    let linePairs = lines |> Seq.pairwise

    //  The length of every line is equal to the number of lines
    test <@ linePairs |> Seq.forall (uncurry(<>)) @>

[<DiamondProperty>]
let ``Diamond output has the correct number of lines`` (letter : char) =
    let numLinesActual = DiamondKata.make letter |> toLines |> Array.length
    let numLinesExpected = (toNum letter) * 2 - 1

    //  The length of every line is equal to the number of lines
    test <@ numLinesActual = numLinesExpected @>