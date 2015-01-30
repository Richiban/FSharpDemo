module RailwayProgramming

type MaybeWorkflow () =
    member this.Return data = Some data
    member this.ReturnFrom result = result
    member this.Bind (result, continuation) = Option.bind continuation result

type Person = { Name : string; Email : string }

let inputData = [1; 2; 3; 4; 5]

let option = MaybeWorkflow ()

let doIt = 
    option {
        let! found = inputData |> List.tryFind ((<=) 5)

        return found.ToString()
    }