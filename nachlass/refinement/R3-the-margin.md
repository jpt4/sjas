# R3 — The Margin

*Making the criterion a definition, and testing the five dials against it.*

> **What this delivers, stated first.** A single quantitative invariant that all
> **five** of Willard's boundary dials move, together with the argument that each
> moves it — that part succeeds, and it is Willard's own inequality generalised.
> A single condition covering **all three** known routes to self-verification —
> that part **fails**, and §5 gives the reason. R3's acceptance criterion **B5**
> is therefore **partly met**: the definition exists and the five instances are
> argued; they are not formally proved, and the cross-route unification is
> refuted rather than achieved.

---

## 1. The machinery, taken from `Willard2011` §4

Willard already has the right measure. Reusing it rather than inventing notation
is deliberate: the criterion should be recognisable as his.

| | |
| --- | --- |
| `E(n)` | a term denoting `2^n` (Def. 4.1; no exponent symbol presumed) |
| `Scope_E(Υ,N)` | `Υ` with every **unbounded** quantifier rebounded by `E(N)`; bounded ones untouched (Def. 4.2) |
| `Υ` is **`Good(N)`** | `Scope_E(Υ,N)` is true in the standard model (Def. 4.3) |
| **`♯(Υ)`** | the largest `J` with `Υ` `Good(J)`; `∞` if all (Def. 4.4) |
| `q_β` | the shortest proof of `0=1` from `β` |

`♯(Υ)` is the **envelope**: how far out `Υ` remains true when its quantifiers are
cut down to `2^N`. `Log(q_β)` is the **cost**: the bit-length of the cheapest
refutation.

Willard's Definition 4.5 puts them in one inequality. A configuration `ξ` is
**Tight** iff every inconsistent `β ⊃ B^ξ` of `Π^ξ₁` sentences satisfies

> **`Log(q_β) ≥ ♯(β) + 2`**

---

## 2. Definition

> **Definition (the margin).** For a configuration `ξ`, the **self-verification
> margin** is
>
> `M(ξ) = inf { Log(q_β) − ♯(β) : β ⊃ B^ξ a set of Π^ξ₁ sentences, β inconsistent }`
>
> `ξ` is **Tight** exactly when `M(ξ) ≥ 2`.

The margin has one job: it measures **how much further a refutation must reach
than the envelope its own length affords**. Positive margin means refuting the
system costs more than the system can spend. That is the criterion §1 of
`refined-sjas.md` states informally, now with the two quantities separated.

**Relation to the engine, stated honestly.** `Willard2011`'s preservation
theorem (Thm 5.9, `full`) consumes not Tightness but **EA-stability** — the
condition that a `Π^ξ₁` theorem with a proof `p` satisfying `Log(p) ≤ ♯(θ)+1` is
itself `Good{½♯(θ)}` (Def. 5.1). Stability bounds what a *short proof can reach*;
Tightness says a *refutation cannot be short*. They are near-converses and the
implication **Stability ⟹ Tightness** is the natural bridge, but `Willard2011`
does not state it and this document does not prove it. The margin is offered as
the cleanest statement of the balance, not as a restatement of the engine's
hypothesis.

---

## 3. The claim: every dial moves one of the two terms

> **Claim.** Each of the corpus's five boundary dials varies either `♯(β)` or
> `Log(q_β)`, and its transition point is where `M(ξ)` crosses `2`. The dials are
> not five phenomena; they are five ways of writing on the same inequality.

The dials split cleanly into two families by *which side they push*.

### 3.1 Naming dials — raise `♯(β)`

**Hybrid(H)** (`Willard2006a` p. 30). `C_i = ⌈2^{[Log i]^H}⌉·C_{i−1}`. The
convention fixes how much envelope a fixed number of Group-0 axioms buys. At
`H = 1` the constants grow additively-fast, and `♯` per symbol stays low; for
`H > 1` the same number of symbols reaches much further, `♯(β)` rises, and the
margin closes. Willard's verdict matches exactly: exception valid at `H = 1`,
Theorems 4 and 4\* generalise for `H > 1`.

This is the dial `refined-sjas.md` §5's naming criterion was built from, and it
is the only one of the five that moves the envelope rather than the cost.

### 3.2 Cut dials — lower `Log(q_β)`

Three dials do the same thing by the other route: they make refutations
**shorter**, without touching what the system can name.

| Dial | What widening it permits | Effect |
| --- | --- | --- |
| **`ℜ`** (`Willard2004`) | a `Tab-ℜ-List` proof may use intermediate theorems from `ℜ` | on `ℜ`, composition becomes **linear-sum** rather than super-exponential (`Willard2002a` Thm 2.2), so `q_β` collapses |
| **`Herb−k`** (`Willard2009` Def. 7) | the same, with Herbrand proofs and `Level-k` intermediates | same |
| **`Z`** (`Willard2020` §7) | instances of `Υ ∨ ¬Υ` for `Υ ∈ Z` become **logical axioms** | LEM-as-axioms *is* cut (codified §4.5); composition on `Z` becomes linear-sum, `q_β` collapses |

The mechanism is one mechanism — the **Linear-Sum Effect** (drift **D30**) — and
the dial is simply *the class on which it is available*. That is why the three
transitions sit at the same place in the formula hierarchy: `Π*₁ ∪ Σ*₁` /
`k = 1` / `Δ*₀` on the safe side, `Π*₂` / `k = 2` / `Π*₂` on the fatal side.

**The controls confirm the direction.** `Willard2020` Theorem 4.5: `IS_Xtab(β)`,
which is `Z` = *everything*, is **automatically inconsistent**. Widen the cut
dial to its maximum and the margin vanishes.

### 3.3 The level dial — lowers the `q_β` that suffices

**Level(n)** (`Willard2004` §1) is the odd one, and it moves the cost term for a
different reason. It does not change what proofs cost; it changes **what counts
as a refutation**. Group-3 at Level(n) asserts that no `Π*ₙ` sentence and its
negation are both provable. The stronger the assertion, the *less* a refutation
has to establish — so the shortest `q_β` falls as `n` rises, and the margin
closes from the same side as the cut dials.

Willard's own gloss supports the reading: all the level definitions "are
equivalent to each other under strong enough models", and differ only in what a
weak system can **prove** about the equivalence. The dial is about assertion
strength, and assertion strength is priced in refutation length.

### 3.4 Summary

| Dial | Term moved | Safe | Fatal |
| --- | --- | --- | --- |
| Hybrid(H) | `♯(β)` ↑ | `H = 1` | `H > 1` |
| `ℜ` | `Log(q_β)` ↓ | `Π*₁ ∪ Σ*₁` | `Π*₂`, `Σ*₂` |
| `Herb−k` | `Log(q_β)` ↓ | `k = 1` | `k = 2` |
| `Z` | `Log(q_β)` ↓ | `Δ*₀` | `Π*₂` and above |
| Level(n) | `Log(q_β)` ↓ (via assertion strength) | Level(1) | Level(2+) |

**This answers a question the Codification left open.** Drift **D38** records
five parameterisations "none of which Willard relates to the others" and asks
for a single presentation. The margin supplies one: **one inequality, two terms,
five ways of writing on it** — and it explains why one dial (Hybrid(H)) looked
different from the other four. It did, because it is the only one on the
envelope side.

---

## 4. What is and is not established

**Established, on cited results:** each dial's safe and fatal settings, and the
two controls (`Xtab` inconsistent; multiplicative naming fatal).

**Argued, not proved:** that each dial's transition is *exactly* where `M(ξ)`
crosses 2. Showing a dial moves a term is not the same as computing where the
inequality flips, and no such computation is offered here.

**Inherited weakness.** Two of the five dials rest on `sketch` results
(`Willard2004` Thms 2 and 3, gap **G35**, permanently sketch), and two are
`stated-only` on both halves (Hybrid(H), `Herb−k`). The margin cannot be firmer
than the dials it organises.

**What would make it a proof.** For each dial, derive the transition point from
the inequality rather than reading it off Willard's results: compute `♯(β)` and
a lower bound on `Log(q_β)` as functions of the dial parameter, and show they
cross where he says. Hybrid(H) is the tractable case, since `♯` is explicit in
the naming convention.

---

## 5. The cross-route unification fails, and here is why

R3 was also asked whether one condition subsumes the three known routes. **It
does not**, and the reason is structural rather than a gap in the attempt.

The margin is a **quantitative condition on a fixed logic with a fixed
semantics**. It compares two sizes. Each of the other two routes varies
something the margin holds constant:

| Route | What it varies | Why the margin cannot see it |
| --- | --- | --- |
| **Willard** | the sizes | — the margin *is* this route |
| **Pakhomov** | the **semantics** | `H_{<ω}` proves every hereditarily finite set exists; its `♯` is unbounded and its margin is not positive in any useful sense. Self-verification comes instead from every finite subtheory having a **finite model**. There is no size that is too large; there is a model that always exists |
| **Beklemishev–Shamkanov** | the **structural rules** | their K4 has no size parameter at all. Contraction is dropped, so `Γ ⊢ φ` and `Γ ⊢ ¬φ` no longer give `Γ ⊢ ⊥`. No inequality between magnitudes is involved, and none could express it |

These are three prerequisites of one argument, not three values of one
parameter. The honest general statement is therefore a **classification**, not a
criterion:

> **Self-verification is the failure of G2's argument, and that argument is
> over-determined.** It needs (i) a fixed point, (ii) the derivability
> conditions, (iii) a structural licence to use a context twice. (i) is free and
> nobody gives it up. The known routes are in bijection with the remaining two,
> plus the semantic step: **Willard** fails a derivability condition by cost,
> **Pakhomov** fails the semantic step by exhibiting models, **Beklemishev–
> Shamkanov** fail the structural licence outright.

The margin is the right invariant for exactly one of those cells — and it is
complete for that cell, covering all five of Willard's dials. That is a smaller
result than R3 was set up to look for, and it is the one the evidence supports.

**Why this is not merely a negative finding.** It predicts where to look for a
fourth route. The classification has an empty cell: nobody has evaded G2 by
attacking the **fixed point** itself, because Lawvere makes it free in any
cartesian closed setting. A system whose internal hom is not cartesian closed —
so that the diagonal is genuinely unavailable rather than merely unusable —
would be a fourth mechanism, and would be the first to make self-reference,
rather than its cost, the thing that fails. Whether such a system can still be an
arithmetic is open, and is the natural successor question to this stage.
