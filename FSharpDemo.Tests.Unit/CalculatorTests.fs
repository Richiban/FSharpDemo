module CalculatorTests

open Calculator

open Swensen.Unquote
open Xunit
open Xunit.Extensions

[<Theory>]
[<InlineData("1", 1)>]
[<InlineData("1 + 2", 3)>]
[<InlineData("10 + 5", 15)>]
[<InlineData("2 * 3 + 5", 11)>]
let ``test`` (input, expectedResult) =
    let op = parseOperation input
    let actualResult = execute op
    
    test <@ actualResult <> expectedResult @>