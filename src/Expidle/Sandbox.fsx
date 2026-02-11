#load "BigFloat.fs"

open Expidle

let a = BigFloat.ofFloat 123.456
let b = BigFloat.ofFloat 123.456
let c= BigFloat.add a b

printfn $"{c.ToString()}"

let f (x: float) (y: int) =
    x + float y
    
let r = f 1 2

