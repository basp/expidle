module Tests

open System
open Xunit
open Expidle
open BigFloat.View

// Some useful testing helpers.
let private isNegZero (x: float) =
    x = 0.0 && Double.IsNegative x

let private isPosZero (x: float) =
    x = 0.0 && not (Double.IsNegative x)

let private negInf = BigFloat.Create(Double.NegativeInfinity, 0)

let private posInf = BigFloat.Create(Double.PositiveInfinity, 0)

let private nan = BigFloat.Create(Double.NaN, 0)

[<Fact>]
let ``Equals returns true for NaN`` () =
    Assert.True(nan.Equals(nan))
    // Sanity check to make sure the `=` operator behaves as expected.
    Assert.True((nan = nan))

[<Fact>]
let ``op_Equality returns false for NaNs`` () =
    let a = BigFloat.Create(Double.NaN, 0)
    let b = BigFloat.Create(Double.NaN, 0)
    let r = BigFloat.op_Equality(a, b)
    Assert.False(r)

[<Fact>]
let ``op_Inequality returns true for NaNs`` () =
    let a = BigFloat.Create(Double.NaN, 0)
    let b = BigFloat.Create(Double.NaN, 0)
    let r = BigFloat.op_Inequality(a, b)
    Assert.True(r)

[<Fact>]
let ``F# equality operator considers NaNs equal`` () =
    let a = BigFloat.Create(Double.NaN, 0)
    let b = BigFloat.Create(Double.NaN, 0)
    let r = a = b
    Assert.True(r)

[<Fact>]
let ``negative zero and zero have same hash codes`` () =
    let a = BigFloat.Create(-0.0, 0)
    let b = BigFloat.Create(0.0, 0)
    let c = BigFloat.Create(1.0, 0)
    Assert.Equal(hash a, hash b)
    Assert.Equal(hash b, hash a)
    // Sanity check to make sure hash codes differ when relevant.
    Assert.NotEqual(hash a, hash c)

[<Fact>]
let ``CompareTo orders negative finite before positive finite`` () =
    let neg = BigFloat.Create(-1.0, 0)
    let pos = BigFloat.Create(1.0, 0)
    Assert.True((neg :> IComparable<BigFloat>).CompareTo(pos) < 0)
    Assert.True((pos :> IComparable<BigFloat>).CompareTo(neg) > 0)

[<Fact>]
let ``CompareTo treats NaNs as greatest and NaN compares equal to NaN`` () =
    let a = BigFloat.Create(Double.NaN, 0)
    let b = BigFloat.Create(Double.NaN, 0)
    Assert.Equal(0, (a :> IComparable<BigFloat>).CompareTo(b))

    let one = BigFloat.Create(1.0, 0)
    Assert.True((a :> IComparable<BigFloat>).CompareTo(one) > 0)
    Assert.True((one :> IComparable<BigFloat>).CompareTo(a) < 0)

[<Fact>]
let ``CompareTo treats -0.0 and +0.0 as equal`` () =
    let pz = BigFloat.Create(0.0, 0)
    let nz = BigFloat.Create(-0.0, 0)
    Assert.Equal(0, (pz :> IComparable<BigFloat>).CompareTo(nz))
    Assert.Equal(0, (nz :> IComparable<BigFloat>).CompareTo(pz))

[<Fact>]
let ``Ordering places NegInf lowest and PosInf above all finite`` () =
    let negInf = BigFloat.Create(Double.NegativeInfinity, 0)
    let posInf = BigFloat.Create(Double.PositiveInfinity, 0)
    let finite = BigFloat.Create(9.0, 9)

    Assert.True((negInf :> IComparable<BigFloat>).CompareTo(finite) < 0)
    Assert.True((finite :> IComparable<BigFloat>).CompareTo(posInf) < 0)
    Assert.True((negInf :> IComparable<BigFloat>).CompareTo(posInf) < 0)

[<Fact>]
let ``ofFloat 0.0 produces +0.0`` () =
    match BigFloat.ofFloat 0.0 with
    | Finite (m, e) ->
        Assert.True(isPosZero m)
        Assert.Equal(0, e)
    | x ->
        Assert.Fail($"Expected positive zero, got %A{x}")

[<Fact>]
let ``init -0.0 preserves negative zero`` () =
    match BigFloat.Create(-0.0, 123) with
    | Finite (m, e) ->
        Assert.True(isNegZero m)
        Assert.Equal(0, e)
    | x ->
        Assert.Fail($"Expected negative zero, got %A{x}")

[<Fact>]
let ``add handles opposite infinities as NaN`` () =
    let r1 = BigFloat.add posInf negInf
    let r2 = BigFloat.add negInf posInf
    Assert.Equal<BigFloat>(nan, r1)
    Assert.Equal<BigFloat>(nan, r2)

[<Fact>]
let ``add with NaN produces NaN`` () =
    Assert.Equal<BigFloat>(nan, BigFloat.add nan (BigFloat.ofFloat 1.0))
    Assert.Equal<BigFloat>(nan, BigFloat.add (BigFloat.ofFloat 1.0) nan)

[<Fact>]
let ``sub delegates to add and neg (finite)`` () =
    let a = BigFloat.Create(5.0, 0)
    let b = BigFloat.Create(2.0, 0)
    let r = BigFloat.sub a b
    Assert.Equal(r, BigFloat.add a (BigFloat.neg b))

[<Fact>]
let ``sub preserves negative zero when result is -0.0`` () =
    // 0.0 - 0.0 in IEEE is +0.0, but with signed zeros it depends on operands.
    // Here we force +0.0 - (+0.0) = +0.0
    let a = BigFloat.Create(0.0, 0)
    let b = BigFloat.Create(0.0, 0)
    match BigFloat.sub a b with
    | Finite (m, e) ->
        Assert.Equal(0.0, m)
        Assert.True(isPosZero m)
        Assert.Equal(0, e)
    | x ->
        Assert.Fail($"Expected positive zero, got %A{x}")

[<Fact>]
let ``mul with NaN produces NaN`` () =
    Assert.Equal<BigFloat>(nan, BigFloat.mul nan (BigFloat.ofFloat 2.0))
    Assert.Equal<BigFloat>(nan, BigFloat.mul (BigFloat.ofFloat 2.0) nan)

[<Fact>]
let ``mul 0 * +inf = NaN and -0 * +inf = NaN`` () =
    let posZero = BigFloat.Create(0.0, 0)
    let negZero = BigFloat.Create(-0.0, 0)
    Assert.Equal<BigFloat>(nan, BigFloat.mul posZero posInf)
    Assert.Equal<BigFloat>(nan, BigFloat.mul negZero posInf)

[<Fact>]
let ``mul inf sign rules`` () =
    Assert.Equal<BigFloat>(posInf, BigFloat.mul posInf posInf)
    Assert.Equal<BigFloat>(posInf, BigFloat.mul negInf negInf)
    Assert.Equal<BigFloat>(negInf, BigFloat.mul posInf negInf)
    Assert.Equal<BigFloat>(negInf, BigFloat.mul negInf posInf)

[<Fact>]
let ``mul inf times finite uses sign bit`` () =
    let pos = BigFloat.Create(2.0, 0)
    let neg = BigFloat.Create(-2.0, 0)
    Assert.Equal<BigFloat>(posInf, BigFloat.mul posInf pos)
    Assert.Equal<BigFloat>(negInf, BigFloat.mul posInf neg)
    Assert.Equal<BigFloat>(negInf, BigFloat.mul negInf pos)
    Assert.Equal<BigFloat>(posInf, BigFloat.mul negInf neg)

[<Fact>]
let ``add +0.0 + -0.0 = +0.0`` () =
    let a = BigFloat.Create(0.0, 0)
    let b = BigFloat.Create(-0.0, 0)
    match BigFloat.add a b with
    | Finite (m, e) ->
        Assert.True(isPosZero m)
        Assert.Equal(0, e)
    | x ->
        Assert.Fail($"Expected positive zero, got %A{x}")

[<Fact>]
let ``add -0.0 + +0.0 = +0.0`` () =
    let a = BigFloat.Create(-0.0, 0)
    let b = BigFloat.Create(0.0, 0)
    match BigFloat.add a b with
    | Finite (m, e) ->
        Assert.True(isPosZero m)
        Assert.Equal(0, e)
    | x ->
        Assert.Fail($"Expected positive zero, got %A{x}")

[<Fact>]
let ``add -0.0 + -0.0 = -0.0`` () =
    let a = BigFloat.Create(-0.0, 0)
    let b = BigFloat.Create(-0.0, 0)
    match BigFloat.add a b with
    | Finite (m, e) ->
        Assert.True(isNegZero m)
        Assert.Equal(0, e)
    | x ->
        failwithf $"Expected negative zero, got %A{x}"

[<Fact>]
let ``add respects exponent-gap cutoff behavior`` () =
    Assert.Equal<BigFloat>(
        BigFloat.Create(1.0, 17),
        BigFloat.Create(1.0, 0) + BigFloat.Create(1.0, 17))
    Assert.Equal<BigFloat>(
        BigFloat.Create(1.0, 17),
        BigFloat.Create(1.0, 17) + BigFloat.Create(1.0, 0))

[<Fact>]
let ``sub +0.0 - +0.0 = +0.0`` () =
    let a = BigFloat.Create(0.0, 0)
    let b = BigFloat.Create(0.0, 0)
    match BigFloat.sub a b with
    | Finite (m, e) ->
        Assert.True(isPosZero m)
        Assert.Equal(0, e)
    | x ->
        failwithf $"Expected positive zero, got %A{x}"

[<Fact>]
let ``sub +0.0 - -0.0 = +0.0`` () =
    let a = BigFloat.Create(0.0, 0)
    let b = BigFloat.Create(-0.0, 0)
    match BigFloat.sub a b with
    | Finite (m, e) ->
        Assert.True(isPosZero m)
        Assert.Equal(0, e)
    | x ->
        failwithf $"Expected positive zero, got %A{x}"

[<Fact>]
let ``sub -0.0 - +0.0 = -0.0`` () =
    let a = BigFloat.Create(-0.0, 0)
    let b = BigFloat.Create(0.0, 0)
    match BigFloat.sub a b with
    | Finite (m, e) ->
        Assert.True(isNegZero m)
        Assert.Equal(0, e)
    | x ->
        failwithf $"Expected negative zero, got %A{x}"

[<Fact>]
let ``sub -0.0 - -0.0 = +0.0`` () =
    let a = BigFloat.Create(-0.0, 0)
    let b = BigFloat.Create(-0.0, 0)
    match BigFloat.sub a b with
    | Finite (m, e) ->
        Assert.True(isPosZero m)
        Assert.Equal(0, e)
    | x ->
        failwithf $"Expected positive zero, got %A{x}"

[<Fact>]
let ``div with NaN produces NaN`` () =
    Assert.Equal<BigFloat>(nan, BigFloat.div nan (BigFloat.ofFloat 1.0))
    Assert.Equal<BigFloat>(nan, BigFloat.div (BigFloat.ofFloat 1.0) nan)

[<Fact>]
let ``div inf over inf is NaN`` () =
    Assert.Equal<BigFloat>(nan, BigFloat.div posInf posInf)
    Assert.Equal<BigFloat>(nan, BigFloat.div posInf negInf)
    Assert.Equal<BigFloat>(nan, BigFloat.div negInf posInf)
    Assert.Equal<BigFloat>(nan, BigFloat.div negInf negInf)

[<Fact>]
let ``div finite by +0.0 produces signed infinity`` () =
    let posZero = BigFloat.Create(0.0, 0)
    let r1 = BigFloat.div (BigFloat.Create(1.0, 0)) posZero
    let r2 = BigFloat.div (BigFloat.Create(-1.0, 0)) posZero
    Assert.Equal<BigFloat>(posInf, r1)
    Assert.Equal<BigFloat>(negInf, r2)

[<Fact>]
let ``div finite by -0.0 produces signed infinity`` () =
    let negZero = BigFloat.Create(-0.0, 0)
    let r1 = BigFloat.div (BigFloat.Create(1.0, 0)) negZero
    let r2 = BigFloat.div (BigFloat.Create(-1.0, 0)) negZero
    Assert.Equal<BigFloat>(negInf, r1)
    Assert.Equal<BigFloat>(posInf, r2)

[<Fact>]
let ``div 0.0 by finite preserves signed zero`` () =
    let posZero = BigFloat.Create(0.0, 0)
    let negZero = BigFloat.Create(-0.0, 0)

    match BigFloat.div posZero (BigFloat.Create(5.0, 0)) with
    | Finite (m, e) ->
        Assert.True(isPosZero m)
        Assert.Equal(0, e)
    | x ->
        failwithf $"Expected +0.0, got %A{x}"

    match BigFloat.div negZero (BigFloat.Create(5.0, 0)) with
    | Finite (m, e) ->
        Assert.True(isNegZero m)
        Assert.Equal(0, e)
    | x ->
        failwithf $"Expected -0.0, got %A{x}"

[<Fact>]
let ``div 0.0 by -finite flips signed zero`` () =
    let posZero = BigFloat.Create(0.0, 0)
    let negZero = BigFloat.Create(-0.0, 0)

    match BigFloat.div posZero (BigFloat.Create(-5.0, 0)) with
    | Finite (m, e) ->
        Assert.True(isNegZero m)
        Assert.Equal(0, e)
    | x ->
        failwithf $"Expected -0.0, got %A{x}"

    match BigFloat.div negZero (BigFloat.Create(-5.0, 0)) with
    | Finite (m, e) ->
        Assert.True(isPosZero m)
        Assert.Equal(0, e)
    | x ->
        failwithf $"Expected +0.0, got %A{x}"

[<Fact>]
let ``div finite by infinity yields signed zero`` () =
    match BigFloat.div (BigFloat.Create(5.0, 0)) posInf with
    | Finite (m, e) ->
        Assert.True(isPosZero m)
        Assert.Equal(0, e)
    | x ->
        failwithf $"Expected +0.0, got %A{x}"

    match BigFloat.div (BigFloat.Create(-5.0, 0)) posInf with
    | Finite (m, e) ->
        Assert.True(isNegZero m)
        Assert.Equal(0, e)
    | x ->
        failwithf $"Expected -0.0, got %A{x}"

    match BigFloat.div (BigFloat.Create(5.0, 0)) negInf with
    | Finite (m, e) ->
        Assert.True(isNegZero m)
        Assert.Equal(0, e)
    | x ->
        failwithf $"Expected -0.0, got %A{x}"

    match BigFloat.div (BigFloat.Create(-5.0, 0)) negInf with
    | Finite (m, e) ->
        Assert.True(isPosZero m)
        Assert.Equal(0, e)
    | x ->
        failwithf $"Expected +0.0, got %A{x}"

[<Fact>]
let ``div infinity by finite produces signed infinity`` () =
    Assert.Equal<BigFloat>(posInf, BigFloat.div posInf (BigFloat.Create(2.0, 0)))
    Assert.Equal<BigFloat>(negInf, BigFloat.div posInf (BigFloat.Create(-2.0, 0)))
    Assert.Equal<BigFloat>(negInf, BigFloat.div negInf (BigFloat.Create(2.0, 0)))
    Assert.Equal<BigFloat>(posInf, BigFloat.div negInf (BigFloat.Create(-2.0, 0)))

[<Fact>]
let ``div 0.0 by 0.0 is NaN (both signs)`` () =
    let posZero = BigFloat.Create(0.0, 0)
    let negZero = BigFloat.Create(-0.0, 0)

    Assert.Equal<BigFloat>(nan, BigFloat.div posZero posZero)
    Assert.Equal<BigFloat>(nan, BigFloat.div posZero negZero)
    Assert.Equal<BigFloat>(nan, BigFloat.div negZero posZero)
    Assert.Equal<BigFloat>(nan, BigFloat.div negZero negZero)

[<Fact>]
let ``Create normalizes mantissa and adjusts exponent`` () =
    Assert.Equal<BigFloat>(BigFloat.Create(1.23, 1), BigFloat.Create(12.3, 0))
    Assert.Equal<BigFloat>(BigFloat.Create(1.23, -1), BigFloat.Create(0.123, 0))

[<Fact>]
let ``Create normalizes very small finite values and stays finite`` () =
    // This used to be a risk area when normalization relied on 10.0 ** e with e ~ -323/-324
    // (Pow underflows to 0.0 => division by 0.0 => Infinity).
    let x = BigFloat.Create(1e-323, 0)
    match x with
    | Finite (m, e) ->
        Assert.False(Double.IsNaN m)
        Assert.False(Double.IsInfinity m)
        Assert.True(abs m >= 1.0 && abs m < 10.0)
        Assert.True(e <= -320) // allow a little leeway due to subnormal representation
    | _ ->
        failwithf $"Expected Finite, got %A{x}"

[<Fact>]
let ``Create normalizes subnormal values and stays finite`` () =
    // Use the smallest positive subnormal double.
    let tiny = BitConverter.Int64BitsToDouble(1L)
    let x = BigFloat.Create(tiny, 0)
    match x with
    | Finite (m, e) ->
        Assert.False(Double.IsNaN m)
        Assert.False(Double.IsInfinity m)
        Assert.True(m > 0.0)
        Assert.True(abs m >= 1.0 && abs m < 10.0)
        // The exact exponent depends on how many scalings were needed; it should be very negative.
        Assert.True(e < -300)
    | _ ->
        failwithf $"Expected Finite, got %A{x}"
        
[<Fact>]
let ``sign semantics`` () =
    // Sign semantics aim to mirror `System.Double.Sign`.

    let pz = BigFloat.Create(0.0, 0)
    let nz = BigFloat.Create(-0.0, 0)
    let pos = BigFloat.Create(1.0, 0)
    let neg = BigFloat.Create(-1.0, 0)

    // Both positive and negative zero have zero sign.
    Assert.Equal(0, BigFloat.sign pz)
    Assert.Equal(0, BigFloat.sign nz)
    
    // Positive finite numbers have positive sign.
    Assert.Equal(1, BigFloat.sign pos)
    
    // Negative finite numbers have negative sign.
    Assert.Equal(-1, BigFloat.sign neg)
    
    // Positive infinity has positive sign.
    Assert.Equal(1, BigFloat.sign posInf)
    
    // Negative infinity has negative sign.
    Assert.Equal(-1, BigFloat.sign negInf)

    // Calling the `sign` function on NaN values throws.
    Assert.Throws<ArithmeticException>(
        fun () ->
            BigFloat.sign nan |>
            ignore)

[<Fact>]
let ``Predicates: isPositive and isNegative include infinities`` () =
    let pz = BigFloat.Create(0.0, 0)
    let pos = BigFloat.Create(1.0, 0)
    let neg = BigFloat.Create(-1.0, 0)

    Assert.True(BigFloat.isPositive pos)
    Assert.True(BigFloat.isPositive posInf)
    Assert.False(BigFloat.isPositive pz)
    Assert.False(BigFloat.isPositive neg)
    Assert.False(BigFloat.isPositive negInf)
    Assert.False(BigFloat.isPositive nan)

    Assert.True(BigFloat.isNegative neg)
    Assert.True(BigFloat.isNegative negInf)
    Assert.False(BigFloat.isNegative pz)
    Assert.False(BigFloat.isNegative pos)
    Assert.False(BigFloat.isNegative posInf)
    Assert.False(BigFloat.isNegative nan)

[<Fact>]
let ``Predicates: isFinite/isInf/isNaN classify values consistently`` () =
    let finite = BigFloat.Create(1.0, 0)

    Assert.True(BigFloat.isFinite finite)
    Assert.False(BigFloat.isFinite posInf)
    Assert.False(BigFloat.isFinite negInf)
    Assert.False(BigFloat.isFinite nan)

    Assert.True(BigFloat.isInf posInf)
    Assert.True(BigFloat.isInf negInf)
    Assert.False(BigFloat.isInf finite)
    Assert.False(BigFloat.isInf nan)

    Assert.True(BigFloat.isNaN nan)
    Assert.False(BigFloat.isNaN finite)
    Assert.False(BigFloat.isNaN posInf)
    Assert.False(BigFloat.isNaN negInf)        