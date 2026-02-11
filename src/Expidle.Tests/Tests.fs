module Tests

open System
open Xunit
open Expidle
open BigFloat.View

let private isNegZero (x: float) =
    x = 0.0 && Double.IsNegative x

let private isPosZero (x: float) =
    x = 0.0 && not (Double.IsNegative x)

let private negInf = BigFloat.Create(Double.NegativeInfinity, 0)
let private posInf = BigFloat.Create(Double.PositiveInfinity, 0)
let private nan = BigFloat.Create(Double.NaN, 0)

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