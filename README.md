# BigFloat
*A cross‑language, IEEE‑inspired arbitrary‑scale floating‑point type for idle games and simulation engines.*

`BigFloat` is the numeric core of **Expidle**, a test‑bed for building an F#‑first game engine/framework tailored to idle and incremental games. It is designed to feel natural in **F#**, predictable in **C#**, and as close as practical to **IEEE‑754 floating‑point semantics**, while supporting numbers far beyond the range of `double`.

---

## ✨ Why BigFloat?

Idle games routinely require:

- numbers in the range of `10^1000` and beyond
- predictable behavior for `NaN`, `±∞`, and signed zero
- deterministic hashing for caching, save‑files, and state diffs
- total ordering for unlocks, thresholds, and UI sorting
- cross‑language scripting (F# and C#)

`BigFloat` provides:

- canonical scientific‑notation representation
- IEEE‑like arithmetic rules
- total ordering (including NaN)
- stable hashing
- F#‑friendly structural equality
- C#‑friendly operator semantics

It is not a drop‑in replacement for `double`, but a practical numeric type for game logic.

---

# ⚖️ Equality & Comparison Semantics
### *This is the most important part of BigFloat.*

`BigFloat` intentionally supports **three different equality worlds**:

1. **.NET / `System.Double.Equals` world**
2. **C# IEEE‑style operator world**
3. **F# structural equality world**

This “semantic tri‑bridge” allows `BigFloat` to behave naturally in both languages while remaining IEEE‑inspired.

---

## 1. `.Equals` and `GetHashCode` (the .NET world)

`.Equals` follows the same semantics as `System.Double.Equals`:

- **All NaNs compare equal to each other**
- **Signed zeros compare equal**
- **Hash codes are consistent with equality**

This ensures:

- dictionaries behave predictably
- memoization and caching work
- NaN does not break hash‑based structures

Example:

```fsharp
let x = BigFloat.NaN
let y = BigFloat.NaN

x.Equals y      // true
x.GetHashCode() // stable, canonical NaN hash
```

---

## 2. `==` operator (the C# world)

C# developers expect IEEE‑754 operator semantics:

```csharp
double.NaN == double.NaN   // false
```

So `BigFloat` mirrors that:

- `NaN == NaN` is **false**
- `NaN != NaN` is **true**

This makes `BigFloat` feel natural in C# numeric code.

---

## 3. `=` operator (the F# world)

F# treats `=` as **structural equality**, not numeric equality:

```fsharp
nan = nan   // true
```

To preserve this expectation:

- `BigFloat`’s F# `=` operator treats all NaNs as equal
- This matches F#’s philosophy and avoids surprising F# developers

---

## Summary Table

| Operation | Behavior | Rationale |
|----------|----------|-----------|
| `Equals` | `NaN = NaN` | Matches .NET & ensures stable hashing |
| `GetHashCode` | All NaNs share one canonical hash | Required for dictionary correctness |
| `==` (C#) | `NaN != NaN` | IEEE‑754 operator semantics |
| `=` (F#) | `NaN = NaN` | F# structural equality semantics |
| `CompareTo` | Total order: `-∞ < finite < +∞ < NaN` | Needed for sorting & game logic |

This design is deliberate and documented to avoid surprises.

---

# 🔢 Arithmetic Semantics

`BigFloat` follows IEEE‑754 rules where practical:

- `0 * ∞ = NaN`
- `∞ / ∞ = NaN`
- `finite / 0 = ±∞` (with correct sign)
- `finite / ∞ = ±0` (signed zero preserved)
- `0 / 0 = NaN`
- `∞ + (-∞) = NaN`

Finite values are normalized to:

- mantissa in the range `1.0 ≤ |m| < 10.0`
- integer exponent

This ensures canonical representation and predictable ordering.

---

# 🧱 Internal Representation

A `BigFloat` is one of:

- `Finite(mantissa, exponent)`
- `PosInf`
- `NegInf`
- `NaN`

Normalization guarantees:

- no `Finite` contains a NaN or infinity mantissa
- mantissa is always in canonical range
- signed zero is preserved

---

# 🧮 Hashing

Special values use fixed, recognizable hex constants:

| Value | Hash |
|-------|------|
| `NaN` | `0x7FC0DEAD` |
| `+∞`  | `0x7F8AFAFA` |
| `-∞`  | `0xFF8AFAFA` |

Finite values hash as a tuple `(mantissa, exponent)` after canonicalization.

This guarantees:

- stable hashing
- no collisions with special values
- consistency with `.Equals`

---

# 🚀 Usage Examples

### F#

```fsharp
let a = BigFloat.Create(1.23, 100)
let b = BigFloat.OfFloat 1e200
let c = a + b
```

### C#

```csharp
var x = BigFloat.OfFloat(42);
var y = BigFloat.PosInf;

var z = x * y;   // +Infinity
```

---

# 🧭 Roadmap

- String parsing & formatting (`Parse`, `TryParse`)
- Configurable normalization bases
- Faster exponent alignment
- Serialization helpers for save‑files
- Interop with scripting languages

---

# 📄 License

MIT — see `LICENSE` for details.

