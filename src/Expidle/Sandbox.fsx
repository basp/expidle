// #load "BigFloat.fs"
// open Expidle
//
// let a = BigFloat.ofFloat 2.0
// let b = BigFloat.ofFloat 3.0
// let c = BigFloat.add a b
//
// c.ToString()

#load "ToyJoy.fs"

open Expidle.ToyJoy

let p = [Int 1; Int 2; Symbol "dup"]
let rt = { defaultRuntime with Queue = p }

let x =
    step rt
    |> Result.bind step
    |> Result.bind step

let y =
    runUntilHalt rt

let testOk =
    match x, y with
    | Ok x, Ok y ->
        let vx = RuntimeStateView.init x
        let vy = RuntimeStateView.init y
        (vx = vy)
    | _ -> false
    