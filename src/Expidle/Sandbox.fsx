#load "BigFloat.fs"

open Expidle

let a: System.IEquatable<BigFloat> = BigFloat.Create(1, 2)
let b = BigFloat.Create(1, 2)

let typedEqualsProperlyCallsObjectEquals = a.Equals(b)
// -> true

let test = a = b

let hashNegativeZeroEqualsHashZero = hash(0.0) = hash(-0.0)
// -> true

let negativeZeroEqualsPositiveZero = 0.0 = -0.0

let h = (hash 0.0) = (hash 0.0)
