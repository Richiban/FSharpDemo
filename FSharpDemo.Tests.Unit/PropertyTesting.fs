module PropertyTesting

open FSharpDemo.PropertyTesting

open Xunit
open System
open Swensen.Unquote

let random = new Random()
let nums = Seq.initInfinite (fun _ -> random.Next(200)) |> Seq.take 1000

[<Fact>]
let ``Test identity`` () =
    for x in nums do
        test <@ add x 0 = x @>

[<Fact>]
let ``Test commutativity`` () =
    for x, y in nums |> Seq.pairwise do
        test <@ add x y = add y x @>

[<Fact>]
let ``Test associativity`` () =
    for x, y, z in Seq.zip3 nums nums nums do
        test <@ ((x |> add y) |> add z) = (x |> add (y |> add z)) @>

[<Fact>]
let ``Test higher order`` () =
    for x in nums do
        test <@ 1 |> List.replicate x |> Seq.fold add 0 = x @>