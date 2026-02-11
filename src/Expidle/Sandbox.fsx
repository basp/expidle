#load "BigFloat.fs"

open Expidle

let a = BigFloat.ofFloat 2.0
let b = BigFloat.ofFloat 3.0
let c = BigFloat.add a b

c.ToString()