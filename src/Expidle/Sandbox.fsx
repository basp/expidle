#load "BigFloat.fs"

open Expidle

let a = BigFloat.init 12 300
let b = BigFloat.init 12 500
let c = a + b
printfn $"{c}"