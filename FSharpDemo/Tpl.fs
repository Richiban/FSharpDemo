module Tpl

open System
open System.Threading
open System.Threading.Tasks

type TaskBuilder() =
    member this.ReturnFrom task = task
    member this.Return result = Task.FromResult result
    member this.Bind (task : 'a Task, continuation : 'a -> 'b Task) =
        task.ContinueWith (fun (t : _ Task) -> (continuation t.Result).Result)

type System.Threading.Tasks.Task with
    static member Sleep (milliseconds : int) =
        Task.Factory.StartNew (fun _ -> Thread.Sleep milliseconds)

let await = TaskBuilder ()

let start = DateTime.Now

printfn "One"

let t = await {
    do! Task.Sleep 2000
    printfn "Two"
    return "Hello world"
}

printfn "Three"

let t' = await {
    do! Task.Sleep 500
    printfn "Four"
    let! s = t
    printfn "Five"

    return s.ToUpper ()
}

printfn "Six"

let finished = DateTime.Now - start

printfn "Finished in %f: %s" (finished.TotalMilliseconds) (t'.Result)