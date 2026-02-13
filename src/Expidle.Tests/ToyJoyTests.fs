module Expidle.Tests.ToyJoyTests

open Xunit
open Expidle.ToyJoy

let testRuntime = {
    Queue = []
    Stack = []
    Env = defaultEnv
    Trace = List.empty
}

[<Fact>]
let ``literal pushing`` () =
    let rt0 = { testRuntime with Queue = [Int 1] }
    match step rt0 with
    | Ok rt1 ->
        let trace = rt1.Trace.Head
        Assert.Equal(Int 1, rt1.Stack.Head)
        Assert.Empty(rt1.Queue)
        Assert.Equal(None, trace.Resolution)
    | Error x ->
        Assert.Fail(x.ToString())

[<Fact>]
let ``dup`` () =
    let r0 = { testRuntime with Queue = [Int 1; Symbol "dup"] }
    let r1 = r0 |> step |> Result.bind step
    match r1 with
    | Ok rt ->
        match rt.Stack with
        | x :: y :: _ ->
            Assert.Equal(Int 1, x)
            Assert.Equal(Int 1, y)        
        | _ ->
            Assert.Fail("Stack is not of length 2")
    | Error (err, _) ->
        Assert.Fail(err.ToString())