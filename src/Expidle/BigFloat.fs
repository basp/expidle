namespace Expidle

// IEEE edge-case choices:
// 0 * inf = NaN
// inf / inf = NaN
// finite / +/-0.0 = +/-inf (with sign rules)
// finite / +/-inf = +/-0.0 (with sign rules)
type [<Struct>] BigFloat =
    private
    | Finite of float * int
    | PosInf
    | NegInf
    | NaN
    
    override x.ToString() =
        match x with
        | Finite (m, e) -> $"%f{m} * 10^%d{e}"
        | PosInf -> "+∞"
        | NegInf -> "-∞"
        | NaN -> "NaN"

module BigFloatUtils =
    open System

    let inline internal addClassify (a: BigFloat) (b: BigFloat) =
        match a, b with
        | NaN, _ -> Choice1Of2 NaN
        | _, NaN -> Choice1Of2 NaN
        | PosInf, NegInf -> Choice1Of2 NaN
        | NegInf, PosInf -> Choice1Of2 NaN
        | PosInf, _ -> Choice1Of2 PosInf
        | _, PosInf -> Choice1Of2 PosInf
        | NegInf, _ -> Choice1Of2 NegInf
        | _, NegInf -> Choice1Of2 NegInf
        | Finite (am, ae), Finite (bm, be) ->
            Choice2Of2 (struct (am, ae, bm, be))
    
    let inline internal mulClassify (a: BigFloat) (b: BigFloat) =
        let inline signBitNegative (x: float) = Double.IsNegative(x)
        
        let inline infTimesFinite (infIsNegative: bool) (m: float) =
            let isNegative = infIsNegative <> signBitNegative m
            if isNegative then Choice1Of2 NegInf
            else Choice1Of2 PosInf
        
        match a, b with
        | NaN, _ -> Choice1Of2 NaN
        | _, NaN -> Choice1Of2 NaN
        
        // IEEE-ish: 0 * inf = NaN
        | Finite (m, _), PosInf when m = 0.0 -> Choice1Of2 NaN
        | Finite (m, _), NegInf when m = 0.0 -> Choice1Of2 NaN
        | PosInf, Finite (m, _) when m = 0.0 -> Choice1Of2 NaN
        | NegInf, Finite (m, _) when m = 0.0 -> Choice1Of2 NaN
        
        // inf * inf (sign rules)
        | PosInf, PosInf -> Choice1Of2 PosInf
        | NegInf, NegInf -> Choice1Of2 PosInf
        | PosInf, NegInf -> Choice1Of2 NegInf
        | NegInf, PosInf -> Choice1Of2 NegInf
        
        // inf * finite (sign rules)
        | PosInf, Finite (m, _) -> infTimesFinite false m
        | Finite (m, _), PosInf -> infTimesFinite false m
        | NegInf, Finite (m, _) -> infTimesFinite true m
        | Finite (m, _), NegInf -> infTimesFinite true m
        | Finite (am, ae), Finite (bm, be) ->
            Choice2Of2 (struct (am, ae, bm, be))

    let inline internal divClassify (a: BigFloat) (b: BigFloat) =
        let inline signBitNegative (x: float) = Double.IsNegative(x)
        
        let inline signedInf (negative: bool) =
            if negative then Choice1Of2 NegInf
            else Choice1Of2 PosInf
        
        let inline signedZeroFromSign (negative: bool) =
            if negative then Choice1Of2 (Finite (-0.0, 0))
            else Choice1Of2 (Finite (0.0, 0))
            
        match a, b with
        | NaN, _ -> Choice1Of2 NaN
        | _, NaN -> Choice1Of2 NaN
        
        // inf / inf = NaN
        | PosInf, PosInf -> Choice1Of2 NaN
        | PosInf, NegInf -> Choice1Of2 NaN
        | NegInf, PosInf -> Choice1Of2 NaN
        | NegInf, NegInf -> Choice1Of2 NaN
        
        // inf / finite = inf
        | PosInf, Finite (m, _) ->
            if m = 0.0 then Choice1Of2 NaN
            else signedInf (signBitNegative m)
        | NegInf, Finite (m, _) ->
            if m = 0.0 then Choice1Of2 NaN
            else signedInf (not (signBitNegative m))

        // finite / inf = signed zero
        | Finite (m, _), PosInf ->
            signedZeroFromSign (signBitNegative m)
        | Finite (m, _), NegInf ->
            signedZeroFromSign (not (signBitNegative m))
            
        // finite / finite = finite
        | Finite (am, _), Finite (bm, _) when bm = 0.0 ->
            if am = 0.0 then Choice1Of2 NaN
            else
                let negative = signBitNegative am <> signBitNegative bm
                signedInf negative
        | Finite (am, ae), Finite (bm, be) ->
            Choice2Of2 (struct (am, ae, bm, be))
        
module BigFloat =   
    open System
    
    let zero = Finite(0.0, 0)
    let negZero = Finite(-0.0, 0)
    let one = Finite(1.0, 0)
    
    let exponentGapCutoff = 16
    
    let private isFinite (v: float) =
        not (Double.IsNaN v) && not (Double.IsInfinity v)    

    let private isNaN = Double.IsNaN
    let private isPosInf = Double.IsPositiveInfinity
    let private isNegInf = Double.IsNegativeInfinity
    let private isNegZero x = x = 0.0 && Double.IsNegative x
    let private isPosZero x = x = 0.0 && Double.IsPositive x
    
    let private zeroOf m =
        if isNegZero m then negZero
        else zero
        
    let private normalizeFinite (mantissa: float, exponent: int) =
        let mutable m = mantissa
        let mutable e = exponent
        
        let inline absM () = abs m
        
        // Scale down in chunks of 1e16 (fast) and then by 10 (fine)
        // keeping intermediate operations in a safe float range.
        while absM () >= 1e16 do
            m <- m / 1e16
            e <- e + 16
            
        while absM () >= 10.0 do
            m <- m / 10.0
            e <- e + 1
            
        // Scale up for tiny magnitudes (subnormals included)
        while absM () > 0.0 && absM () < 1e-16 do
            m <- m * 1e16
            e <- e - 16
            
        while absM () > 0.0 && absM () < 1.0 do
            m <- m * 10.0
            e <- e - 1            

        // Guard against rounding producing exactly 10.0.        
        if abs m >= 10 then
            m <- m / 10.0
            e <- e + 1            
        
        Finite (m, e)
        
    let private normalize x =
        match x with        
        | Finite (mantissa, _)
            when mantissa = 0.0 ->
            zeroOf mantissa
        | Finite (mantissa, _)
            when isNegInf mantissa ->
            NegInf
        | Finite (mantissa, _)
            when isPosInf mantissa ->            
            PosInf
        | Finite (mantissa, _)
            when isNaN mantissa ->
            NaN
        | Finite (mantissa, exponent)
            when (isFinite mantissa) ->
            normalizeFinite (mantissa, exponent)
        | nonFinite -> nonFinite

    let init m e =
        match m with
        | m when isNaN m -> NaN
        | m when isPosInf m -> PosInf
        | m when isNegInf m -> NegInf
        | m when m = 0.0 -> zeroOf m
        | _ -> Finite(m, e) |> normalize
        
    let ofFloat x = init x 0        

    let neg x =
        match x with
        | Finite (m, e) -> init (-m) e
        | PosInf -> NegInf
        | NegInf -> PosInf
        | NaN -> NaN
    
    let add a b =
        let addFinite (am, ae) (bm, be) =
            match am, bm with
            | 0.0, 0.0 -> init (am + bm) 0
            | 0.0, _ -> b
            | _, 0.0 -> a
            | _ ->
                let d = ae - be
                if d >= 0 then
                    if d > exponentGapCutoff then a
                    else
                        let m = am + bm * (10.0 ** float -d)
                        init m ae
                else
                    if -d > exponentGapCutoff then b
                    else
                        let m = am * (10.0 ** float d) + bm
                        init m be
        match BigFloatUtils.addClassify a b with
        | Choice1Of2 special ->
            special
        | Choice2Of2 (am, ae, bm, be) ->
            addFinite (am, ae) (bm, be)

    let sub a b = add a (neg b)
    
    let mul a b =
        let mulFinite (am, ae) (bm, be) =
            init (am * bm) (ae + be)
        match BigFloatUtils.mulClassify a b with
        | Choice1Of2 special ->
            special
        | Choice2Of2 (am, ae, bm, be) ->
            mulFinite (am, ae) (bm, be)
            
    let div a b =
        let divFinite (am, ae) (bm, be) =
            init (am / bm) (ae - be)
        match BigFloatUtils.divClassify a b with
        | Choice1Of2 special ->
            normalize special
        | Choice2Of2 (am, ae, bm, be) ->
            divFinite (am, ae) (bm, be)
        
    module View =
        let (|Finite|PosInf|NegInf|NaN|) (x: BigFloat) =
            match x with
            | BigFloat.Finite (m, e) -> Finite (m, e)
            | BigFloat.PosInf -> PosInf
            | BigFloat.NegInf -> NegInf
            | BigFloat.NaN -> NaN

        let (|Finite|_|) (x: BigFloat) =
            match x with
            | BigFloat.Finite (m, e) -> Some (m, e)
            | _ -> None
            
        let (|Special|_|) (x: BigFloat) =
            match x with
            | BigFloat.PosInf -> Some "PosInf"
            | BigFloat.NegInf -> Some "NegInf"
            | BigFloat.NaN -> Some "NaN"
            | _ -> None
        
type BigFloat with
    static member Zero = BigFloat.zero
    static member NegZero = BigFloat.negZero
    static member One = BigFloat.one
    static member Create (mantissa: float, exponent: int) =
        BigFloat.init mantissa exponent
    static member OfFloat (x: float) =
        BigFloat.ofFloat x
    static member (+) (a, b) =
        BigFloat.add a b
    static member (-) (a, b) =
        BigFloat.sub a b
    static member (*) (a, b) =
        BigFloat.mul a b
    static member (/) (a, b) =
        BigFloat.div a b
