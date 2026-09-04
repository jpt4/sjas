# R3 — The Margin

*Making the criterion a definition, and testing the five dials against it.*

> **What this delivers, stated first — revised 2026-09-02 after a full
> page-image re-reading of `Willard2011` §§4–5.**
>
> - The **definition** holds. It is not a generalisation of Willard's Tightness,
>   as earlier drafts claimed; it **is** Tightness, restated as an infimum.
> - **Three** of the five dials are now *proved* to move it, not merely argued —
>   §3.0 gives the one-line reason none of the drafts had.
> - **Level(n)'s** case does **not** hold. §3.3 retracts it.
> - **Hybrid(H)'s** term remains unidentified, and the withdrawal is now
>   attributed to Willard, who makes the same move himself at Eq. (20).
> - The **A-Stability ⟹ Tightness** bridge holds and is completed (§1.2), but it
>   re-derives a conclusion Willard already has (Thm 5.11).
> - The cross-route unification **fails**; §5.
>
> **B5 is partly met**, and the honest count is three of five, not five of five.
> §6 states the verdict in full.

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
| `q_β` | the shortest proof of `0=1` from `β` (Def. 4.5) |

`♯(Υ)` is the **envelope**: how far out `Υ` remains true when its quantifiers are
cut down to `2^N`. `Log(q_β)` is the **cost**.

Three points from the page images (printed pp. 12–14) that the drafts written
from extracted text did not carry, and that decide most of what follows:

1. **`E(n)` denotes a fixed value.** Def. 4.1: `E(n)` is "some term in
   Definition 3.4's language `L^ξ` that represents the value `2^n`", by any
   reasonable method. The *term* may be long or short depending on what the
   language can name; the *value* is `2^n` regardless. So `Scope_E`, `Good(N)`
   and `♯` are fixed once the sentences are fixed.
2. **`♯` is a function of the sentences alone.** Def. 4.3 makes `Good(N)` a
   claim about truth in the Standard-M model; Def. 4.4 makes `♯(θ)` the largest
   `J` at which every sentence of `θ` is `Good(J)`. Neither mentions the
   deduction method `d`. **`♯` is `d`-independent.** §3.0 turns this into a
   proof.
3. **`Good(N)` runs opposite ways on `Π` and `Σ`.** Footnote 6: for `Π^ξ₁`
   sentences `Good(N)` is *weaker* than `Good(∞)`; for `Σ^ξ₁` it is *stronger*,
   because `∀x φ` implies `∀x<E(N) φ` but `∃x<E(N) φ` implies `∃x φ`. This is why
   Defs. 5.1 and 5.3 are not mirror images.

Willard's Definition 4.5 puts the two quantities in one inequality. Verbatim from
the page image: `B^ξ` is "comprised exclusively of `Π^ξ₁` sentences", `β ⊃ B^ξ`
is "a second axiom system, comprised also of `Π^ξ₁` sentences, that (unlike
`B^ξ`) can possibly be inconsistent", `q_β` is "the shortest proof of `0 = 1`
from `β`", and `ξ` is **Tight** iff *every inconsistent* such `β` satisfies

> **`Log(q_β) ≥ ♯(β) + 2`**  (17)

### 1.1 What `q_β` and `Log(q_β)` actually measure

Definition 4.5 says only "the shortest proof of `0 = 1` from `β`", so the measure
has to be read off the corpus's standing conventions. It is worth pinning,
because the margin is a difference of two quantities and one of them is
encoding-relative.

- **`q_β` is a Gödel number**, not a symbol count. This is forced by the notation
  — one takes `Log` of it — and confirmed by the corpus's convention:
  `Willard2007-APAL` Definition 9, "the length (or perhaps one should say
  **bit-length**) of a proof `p` is represented by the quantity `Log₂(p)` when
  `p` is written in a binary notation… `|p|` denotes `Log₂(p)`."
- **`Log` is base 2, rounded down** (`Willard2000-TAB` Def. 1;
  `Willard2007-APAL` Def. 2). So **`Log(q_β)` is the bit-length of the Gödel
  number** of the cheapest refutation.
- **"Shortest" means least Gödel number.** The corpus's **Hilbert-line**
  preservation proofs are minimal-counterexample arguments on exactly that
  quantity — "take `P` minimising `Θ(P)`", then construct a smaller `R` with
  `Θ(R) < Θ(P)` (codified §6.9: `Willard2001` Thms 3.4 and 4.3, `Willard2006a`
  Thm 3, `Willard2016` Thm 6.7). An earlier version of this bullet said *every*
  proof in the corpus runs that way; the **tableaux** line instead uses
  `θ`-compactification (`Willard2005` Thm 1, codified §6.4), which §6.9 names as
  the contrast. The reading of `q_β` as a least Gödel number is unaffected —
  Def. 4.5 says "shortest proof" and the Hilbert-line usage fixes the sense —
  but the support for it is one line's practice, not the whole corpus's.

**The bridge to symbol count is an explicit hypothesis, not an assumption.**
`Willard2011` Appendix D imposes the **Conventional Tableaux Encoding
Requirement**: the Gödel number of a tableaux proof with `J` function symbols
"must be [at] least as large as `32^J`" — so a proof with `J` function symbols
"will contain at least `2J` logical symbols and thus employ at least `5J` bits"
(footnote 23, image-verified; the printed text omits the "at"). That is what
makes `Log(q_β)` a faithful proxy for the proof as written, and Willard notes
"all the usual methods for encoding semantic tableaux proofs satisfy this
criteria".

**Consequence for the margin: it is encoding-relative.** `♯(β)` is
model-theoretic and encoding-free; `Log(q_β)` is not. `M(ξ)` is therefore
well-defined only relative to a Gödel numbering, and the Conventional Encoding
Requirement is what fixes one.

**But note which way it cuts.** The requirement is `Gödel number ≥ 32^J`, i.e.
`Log(q_β) ≥ 5J` — a **lower** bound, which excludes *over-compressed* encodings
that would understate a proof's cost. An earlier draft claimed the opposite:
that it guards against a wasteful encoding inflating `Log(q_β)` and manufacturing
margin. It does not, and a wasteful encoding satisfies it trivially. That hazard
is real and **remains unguarded** by the cited requirement; a full treatment
would need an upper bound as well, which the corpus does not supply.
This is the same condition the corpus's coding-density family (drift **D25**,
obligation **O33**) exists to pin down, and it is why `Willard2005`'s Eq. (20),
`U-Height(p) < (1/5)·Log₂(p)`, is stated for "any other reasonable encoding
methodology" rather than for one scheme.

Three distinct size measures coexist in the corpus and should not be conflated:
the **Gödel bit-length** `Log₂(p)` used here; **`χ(p)`**, the count of logical
symbols (`Willard2004` §6, where `ℑ(H) = Σ χ(p_i)`); and **`U-Height`**, a count
of U-Grounding function symbols along a branch (`Willard2005` §5.2).

An earlier version of this paragraph closed "the encoding requirement is
precisely what keeps them within constant factors." **It does not**, and that
contradicts the correction two paragraphs above: a lower bound alone relates the
measures in one direction only. It gives `Log₂(q_β) ≥ 5J`, so bit-length cannot
*understate* symbol count; nothing in the corpus stops it overstating. Keeping
the three within constant factors of one another would need an upper bound too.

---

## 2. Definition

> **Definition (the margin).** For a configuration `ξ`, the **self-verification
> margin** is
>
> `M(ξ) = inf { Log(q_β) − ♯(β) : β ⊃ B^ξ a set of Π^ξ₁ sentences, β inconsistent }`
>
> where `q_β` is the **least Gödel number** of a proof of `0=1` from `β`, `Log`
> is base-2 with downward rounding, and the Gödel numbering satisfies the
> Conventional Encoding Requirement of §1.1.
>
> `ξ` is **Tight** exactly when `M(ξ) ≥ 2`.

The margin has one job: it measures **how much further a refutation must reach
than the envelope its own length affords**. Positive margin means refuting the
system costs more than the system can spend. That is the criterion §1 of
`refined-sjas.md` states informally, now with the two quantities separated.

**This is a restatement, not a generalisation.** Earlier drafts of this document
said the margin "generalises" Def. 4.5. It does not: `∀β. A(β) ≥ 2` and
`inf_β A(β) ≥ 2` are the same statement. The infimum form is more convenient —
it makes the quantity a *number* attached to `ξ`, which is what lets §3 ask which
dial moves it — but nothing has been generalised, and the claim is withdrawn.

### 2.1 Why the threshold is 2, which the corpus explains and no draft carried

The `2` looks arbitrary. It is not, and the reason is the best thing in this
part of the corpus.

Lemma 4.6's proof (printed pp. 14–15, image-verified) computes the margin of the
one `β` that matters. Let `Ψ = SelfRef(B^ξ,d)`, the self-referential axiom, and
suppose it is **false** in the Standard-M model — so a proof `q` of `0=1` from
`B^ξ + Ψ` exists. Then

> `Log(q) = ♯(Ψ) + 1`   (19)

because `Ψ` is `∀p ¬Prf(⌜0=1⌝,p)`, whose `Scope_E(Ψ,J)` asserts that no proof of
`0=1` lies below `2^J` — true exactly while `J` stays under `Log(q)`. Willard's
footnote 7 puts it directly: "`♯(Ψ)` will equal `Log(q) − 1` when `q` denotes the
shortest proof of `0=1` from `B^ξ + SelfRef(B^ξ,d)`." And

> `♯(B^ξ + Ψ) = ♯(Ψ)`   "because all of `B^ξ`'s axioms are true under the
> Standard-M model"

so (19) becomes `Log(q) = ♯(B^ξ + Ψ) + 1`  (20).

**So the self-referential axiom sits at margin exactly 1.** Tightness demands
margin `≥ 2` of *every* inconsistent `β`. The axiom that says "I am consistent"
is therefore excluded from being inconsistent by exactly one bit — and Lemma 4.6
is that one-line contradiction, nothing more.

Two things follow that change how this document should be read.

1. **The threshold is not a tuning constant.** It is `1 + (the margin the fixed
   point achieves when false)`. Any invariant of this shape would have the same
   `+1` gap; the `2` is forced.
2. **Willard himself argues that true axioms do not lower `♯`.** Equation (20)'s
   justification is exactly the reasoning §3.1 uses to withdraw the Hybrid
   side-assignment. That withdrawal is therefore not this Refinement's
   inference — it is Willard's own move, and §3.1 is corrected to attribute it.

**Relation to the engine.** `Willard2011`'s preservation theorem (Thm 5.9,
`full`) consumes **EA-stability**. From the page images (printed pp. 15–16):

- an **R-View** `θ` is "any recursively enumerable (r.e.) set of `Π^ξ₁`
  sentences … An R-View does not need to be valid under the Standard-M model.
  It only needs to be r.e."; `RE-Class(ξ)` is the set of all of them, "both
  valid and invalid";
- `ξ` is **A-Stable** iff for every `θ ∈ RE-Class(ξ)`: if `Υ` is a `Π^ξ₁`
  theorem of `θ ∪ B^ξ` via a proof `p` with `Log(p) ≤ ♯(θ)+1`, then `Υ`
  satisfies `Good{½♯(θ)}` (Def. **5.1**);
- `ξ` is **E-Stable** iff the `Σ^ξ₁` counterpart holds, with
  `Good{½⌊Log(p)⌋−1}` (Def. **5.3**);
- `ξ` is **EA-stable** iff both (Def. **5.5**); Thm 5.9's dependencies are
  Defs. 5.5 and 5.7, not 5.1.

### 2.2 A-Stability implies Tightness — the sketch, completed

An earlier draft offered this as a sketch. It completes, and the gap it left
open closes cleanly.

> **Claim.** If `ξ` is A-Stable then `ξ` is Tight.

*Proof.* Let `β ⊃ B^ξ` be any inconsistent set of `Π^ξ₁` sentences.

*Step 1 — reduce to an r.e. set.* A-Stability quantifies over **r.e.** `θ`;
Tightness quantifies over **every** inconsistent `β`, r.e. or not. Let `q_β` be
the shortest proof of `0=1` from `β`, and let `β₀` be `B^ξ` together with the
finitely many axioms of `β` that `q_β` actually uses. Then `β₀` is r.e., `β₀` is
inconsistent, and `q_{β₀} = q_β` — the same proof witnesses both, and no shorter
proof exists from the smaller set. Since `β₀ ⊆ β` and `♯` of a set is a minimum
over its members, `♯(β₀) ≥ ♯(β)`.

*Step 2 — instantiate.* Apply (∗) with `θ = β₀`, `Υ = (0{=}1)`, `p = q_{β₀}`.
Note `θ ∪ B^ξ = β₀`, and `0=1` is a `Δ^ξ₀` — hence `Π^ξ₁` — theorem of it. Were
`Log(q_{β₀}) ≤ ♯(β₀)+1`, A-Stability would give `0=1` `Good{½♯(β₀)}`. But by
the Special Note to Def. 4.2, `Scope_E(0{=}1, N) ≡ (0{=}1)` for every `N`, and
`0=1` is false in the Standard-M model, so `0=1` is `Good(N)` for **no** `N`.
Contradiction. Hence `Log(q_{β₀}) ≥ ♯(β₀)+2`, both sides being integers.

*Step 3 — transfer.* `Log(q_β) = Log(q_{β₀}) ≥ ♯(β₀)+2 ≥ ♯(β)+2`. ∎

**Two caveats, and they matter.** The argument needs `♯(β₀)` to be a genuine
integer; a `β` false at a very small witness can drive `♯` below 0, where
`Good{½♯}` is degenerate, and the corpus does not fix a convention for that.
And the conclusion is **not new**: Remark 5.2 announces "Theorem 5.11 will show
the presence of A-stability, alone, is sufficient for constructing
self-justifying systems", and Theorem 6.3 uses it. What this argument supplies
is a *shorter route* to that conclusion, through Tightness and Lemma 4.6 rather
than through 0-stability and Appendix C. Presenting it as a new result would be
wrong; presenting it as a simplification is the Refinement's actual remit.

---

## 3. The claim under test, and how much of it survives

> **The claim, as this document originally made it.** Each of the corpus's five
> boundary dials varies either `♯(β)` or `Log(q_β)`, and its transition point is
> where `M(ξ)` crosses `2`. The dials are not five phenomena; they are five ways
> of writing on the same inequality.
>
> **Verdict (2026-09-02).** Three of the five survive; Level(n) is retracted
> (§3.3) and Hybrid(H) is undetermined (§3.1). No dial's transition point has
> been *computed* from the inequality — the second half of the claim is
> untested throughout, for all five. §6 states this in full.

**A caution the earlier drafts lacked.** `M(ξ)` is a *difference*. Showing that
a dial lowers `Log(q_β)` is not yet a claim about the margin: if `♯(β)` falls by
as much, the margin is unchanged. Every argument below must therefore say what
happens to **both** terms. Three of the five survive that test; one fails it;
one is undetermined.

### 3.0 The dials that change only the apparatus, and why they are provable

> **Lemma (`♯` is `d`-independent).** Let `ξ = (L^ξ, Δ^ξ₀, B^ξ, d, G)` and
> `ξ′ = (L^ξ, Δ^ξ₀, B^ξ, d′, G)` differ only in the deduction method. Then for
> every set `β` of `Π^ξ₁` sentences, `♯_ξ(β) = ♯_{ξ′}(β)`.
>
> *Proof.* `E(n)` denotes the value `2^n` (Def. 4.1); `Scope_E(Υ,N)` is a
> syntactic operation on `Υ` (Def. 4.2); `Good(N)` is truth of `Scope_E(Υ,N)` in
> the Standard-M model (Def. 4.3); `♯(β)` is the largest `J` at which every
> member of `β` is `Good(J)` (Def. 4.4). None of the four mentions `d`. ∎

Trivial, and decisive. **Any dial that varies only the deduction method moves
`Log(q_β)` and cannot move `♯(β)`** — so for such a dial, "lowers the cost
term" *is* "lowers the margin", with no second check required.

That covers three of the five: **`ℜ`** (`Tab-ℜ-List` deduction), **`Herb−k`**
(Herbrand deduction with Level-`k` intermediates, `Willard2009` Def. 7), and
**`Z`** (which `Υ ∨ ¬Υ` instances are *logical axioms* — a property of the
apparatus, not of `β`). §3.2 gives their settings.

The other two — Hybrid(H) and Level(n) — change the **sentences**, so both terms
move and the second check is mandatory. §3.1 and §3.3 apply it, and only one of
them survives.

### 3.1 The naming dial — side undetermined

**Hybrid(H)** (`Willard2006a` p. 30, image-verified). Willard introduces
"hybridized naming conventions that lie midway between the additive and
multiplicative conventions": for fixed `H` **and any `i ≥ 3`**,
`C_i = ⌈2^{[Log(i)]^H}⌉·C_{i−1}`. He offers it explicitly as a unifier — "This
convention can unify the formalisms of Theorems 3 and 4" — which is a point in
favour of this document's project that earlier drafts did not cite. So

> `Log₂ C_n ≈ Σ_{i≤n} (Log i)^H ≈ n·(log n)^H`

**Two things an earlier draft of this subsection got wrong, both material.**

*First, the arithmetic.* That draft said "at `H = 1` the constants grow
additively-fast". They do not. Additive naming gives `Log₂ C_n = n`; Hybrid(1)
gives `Θ(n log n)` — a full logarithmic factor above it — and no *fixed* `H`
reaches the multiplicative `2^n`. Willard's own "midway between the additive and
multiplicative conventions" is loose, and this document adopted it as though
exact.

The consequence is severe for `refined-sjas.md` §5's criterion, which asks that
a length-`L` proof denote nothing whose encoding exceeds `O(L)`. Hybrid(1) is the
**positive** case and is not `O(L)`; Hybrid(2) is the negative case and is also
quasi-linear. **The `O(L)` criterion misclassifies the positive case and cannot
locate this transition in either direction.** The real boundary sits between
`n log n` and `n (log n)²` — a single log factor — which no criterion of that
coarseness can express. §5 is corrected accordingly.

*Second, the side.* That draft assigned Hybrid(H) to the envelope side and made
that the headline explanation of why it "looked unlike the other four". **No
argument was given, and there is a difficulty — one Willard states himself.**
`♯` is purely model-theoretic (`Good(N)` = `Scope_E(Υ,N)` true in the standard
model), and the naming axioms are *true* sentences, so they are `Good(∞)` and
cannot lower `♯(β)`. That is precisely the justification Willard gives for
Equation (20): `♯(B^ξ + Ψ) = ♯(Ψ)` "because all of `B^ξ`'s axioms are true under
the Standard-M model" (printed p. 15, image-verified). The reasoning is his, not
this Refinement's, and §2.1 records it.
Whatever the naming convention does to the margin, it does indirectly — most
plausibly by changing which `β ⊃ B^ξ` are inconsistent and how cheaply, which is
the **cost** side. That reading also agrees with `refined-sjas.md` §4's account
of the same boundary ("a short proof has named a long object … the system can
assert — cheaply — the existence of something big enough to encode a refutation
of itself"), which is a statement about `Log(q_β)`.

**Disposition: the Hybrid dial's side-assignment is withdrawn.** It moves the
margin; which term it moves is not established here.

### 3.2 Cut dials — lower `Log(q_β)`

Three dials do the same thing by the other route: they make refutations
**shorter**, without touching what the system can name.

| Dial | What widening it permits | Effect |
| --- | --- | --- |
| **`ℜ`** (`Willard2004`) | a `Tab-ℜ-List` proof may use intermediate theorems from `ℜ` | on `ℜ`, composition becomes **linear-sum** rather than super-exponential (`Willard2002a` Thm 2.2, **`sketch`**), so `q_β` collapses |
| **`Herb−k`** (`Willard2009` Def. 7) | the same, with Herbrand proofs and `Level-k` intermediates | same |
| **`Z`** (`Willard2020` §7) | instances of `Υ ∨ ¬Υ` for `Υ ∈ Z` become **logical axioms** | LEM-as-axioms *is* cut (codified §4.5); composition on `Z` becomes linear-sum, `q_β` collapses |

The mechanism is the **Linear-Sum Effect** (drift **D30**) in all three cases,
and the dial is *the class on which it is available*.

**This is as far as the claim may go, and an earlier draft went further.** That
draft said the three transitions "sit at the same place in the formula
hierarchy". They do not — the safe settings are `Π*₁ ∪ Σ*₁`, `k = 1` and `Δ*₀`,
and `Δ*₀` is strictly below the others. More seriously, treating `ℜ` and `Z` as
interchangeable is the merge `codified-sjas.md` §8.1 **expressly forbids**:
`ℜ` parameterises which *intermediate theorems* a TabList proof may use, `Z`
which *LEM instances* are logical axioms, and the claim that a result transfers
from one to the other **is the corpus's open problem** (drift **D34**,
obligation **O46**, gap **G22**). Asserting the transfer as settled inside an
argument for the margin would make the margin's best evidence circular on the
corpus's central open question. The three dials are grouped here by *mechanism*;
they are not identified.

**A limiting case, offered as illustration and not as evidence.**
`Willard2020` Theorem 4.5 (`sketch`) has `IS_Xtab(β)` — the `Z` dial at
*everything* — automatically inconsistent, under a hypothesis on `β` that must
be carried. It is consistent with the direction claimed here. It does not
*confirm* it: an inconsistency at the extreme setting has several available
explanations, and §4 records the withdrawal.

### 3.3 The level dial — the case fails, and is retracted

**Level(n)** (`Willard2004` §§1–2) parameterises what the self-referential
Group-3 axiom asserts. Two successive drafts tried to place it on the cost side;
both are now withdrawn, the second for a reason the first did not reach.

*The first draft* said Level(n) "does not change what proofs cost; it changes
what counts as a refutation", then concluded `q_β` falls — inconsistent, since
`q_β` is the least Gödel number of a proof of the *fixed* target `0=1`.

*The second draft* said Level(n) strengthens the Group-3 axiom, hence the base
`B^ξ`, hence makes `0=1` cheaper to derive. **Two things are wrong with it.**

1. **Group-3 is not in `B^ξ`.** Definition 4.5 has `B^ξ` as the base and Lemma
   4.6 adds `SelfRef(B^ξ,d)` *on top* of it — the page image is explicit that
   `B^ξ` is the base "comprised exclusively of `Π^ξ₁` sentences" and that the
   system under test is `B^ξ + SelfRef(B^ξ,d)`. Level(n) varies the added
   axiom, not the base.
2. **Both terms move together, and at the critical `β` they move in lockstep.**
   Level(n) changes `Ψ`, a *sentence*, so §3.0's lemma does not apply and `♯`
   moves too. Worse for the argument: §2.1's Equation (20) says that at
   `β = B^ξ + Ψ` the two are **equal up to the constant**,
   `Log(q) = ♯(β) + 1`, *whatever* `Ψ` is. So at the one `β` the drafts were
   reasoning about, the margin is identically 1 and the level dial cannot move
   it at all.

**Disposition: the Level(n) instance is retracted.** The dial's positive and
negative settings are Willard's (`Willard2004`, Level(1) works, Level(2+)
fails, both `full` modulo G35's deferred `T* ≤ N²` step) and are not in doubt.
What is retracted is this document's claim to have *located* the dial on the
margin. Since Level(n) alters `Ψ`, it changes which *other* `β ⊃ B^ξ` are
inconsistent and by how much — an effect on the infimum over the whole class,
which none of the drafts examined and this one does not either.

### 3.4 Summary

| Dial | What it varies | Term moved | Safe | Fatal |
| --- | --- | --- | --- | --- |
| **`ℜ`** | the apparatus `d` | `Log(q_β)` ↓, **proved** (§3.0) | `Π*₁ ∪ Σ*₁` | `Π*₂`, `Σ*₂` |
| **`Herb−k`** | the apparatus `d` | `Log(q_β)` ↓, **proved** (§3.0) | `k = 1` | `k = 2` |
| **`Z`** | the apparatus `d` | `Log(q_β)` ↓, **proved** (§3.0) | `Δ*₀` | `Π*₂` and above |
| Hybrid(H) | the language and base | **undetermined** (§3.1) | `H = 1` | `H > 1` |
| Level(n) | the axiom `Ψ` | **retracted** (§3.3) | Level(1) | Level(2+) |

**This answers a question the Codification left open.** Drift **D38** records
five parameterisations "none of which Willard relates to the others" and asks
for a single presentation. The margin supplies a partial one: **one inequality, two terms, and three dials
provably writing on the same term.**

The split that survives is not the "envelope versus cost" one the drafts kept
reaching for. It is **apparatus versus sentences**: a dial that varies only `d`
cannot touch `♯`, so it moves the margin by construction (§3.0); a dial that
varies the sentences moves both terms and needs an argument nobody has given.
D38's request is answered for three of five, sharpened for Hybrid(H), and
reopened for Level(n).

---

## 4. What is and is not established

**Taken from cited results:** each dial's safe and fatal settings — with the
statuses of §4's second paragraph, not as established fact. The two "controls"
an earlier draft offered here (`Xtab` inconsistent; multiplicative naming fatal)
have been **withdrawn as evidence**: they are compatible with several
explanations, including two this document gives elsewhere, and
`refined-sjas.md` §2.3b sets out why. `Willard2020` Thm 4.5 is moreover `sketch`
and carries a hypothesis on `β` that the draft dropped.

**Argued, not proved:** that each dial's transition is *exactly* where `M(ξ)`
crosses 2. Showing a dial moves a term is not the same as computing where the
inequality flips, and no such computation is offered here.

**Retracted outright.** The Level(n) instance (§3.3). Four remain claimed, three
of them proved.

**Inherited weakness.** Per `codified-sjas.md` §8.1, exactly **one** of the five
dials is `full` on both halves — and it is Level(n), the one whose instance has
just been retracted, so the margin's best-supported dial is the one it can no
longer place:

| Dial | Status of its two settings |
| --- | --- |
| `ℜ` | `sketch` / `sketch` — `Willard2004` Thms 3 and 2, gap **G35**, permanently sketch |
| Hybrid(H) | `stated-only` / `stated-only` |
| `Herb−k` | `stated-only` / `stated-only` |
| `Z` | `stated-only`; **`Π*₁` open** — and its one supporting control, Thm 4.5, is itself `sketch` |
| Level(n) | `full` / `full` (modulo G35's deferred `T* ≤ N²` step) |

An earlier draft said "two of the five rest on sketch results", counting
`Willard2004` Thms 2 and 3 as two dials when they are the two halves of one, and
omitted the `Z` dial from the accounting entirely. The margin cannot be firmer
than the dials it organises, and four of the five are not firm.

**Encoding-relativity is a live caveat, not a formality.** Because `Log(q_β)`
is measured on Gödel numbers, any derivation of a transition point must carry the
encoding hypothesis explicitly. `Willard2011`'s own Appendix D does this; a
future proof of the instances must too.

**What would make it a proof.** For each dial, derive the transition point from
the inequality rather than reading it off Willard's results: compute `♯(β)` and
a lower bound on `Log(q_β)` as functions of the dial parameter, and show they
cross where he says. Hybrid(H) is the tractable case, since `♯` is explicit in
the naming convention.

---

## 5. The cross-route unification fails, and here is why

R3 was also asked whether one condition subsumes the known routes. **It does
not** — but an earlier version of this section got the reason and the roster
wrong, and both corrections narrow the result.

### 5.1 Three routes, and only two of them reach a self-verifying theory

The margin is a **quantitative condition on a fixed logic with a fixed
semantics**. It compares two sizes. The other routes vary something it holds
constant:

| Route | What it varies | Does it yield a self-verifying theory? |
| --- | --- | --- |
| **Willard** | the sizes | **Yes.** The margin *is* this route |
| **Pakhomov** | the **semantics** | **Yes.** `H_{<ω}` proves `Con^pred(H_{<ω})`, a predicate-only `Π^pred₁` consistency sentence. Like Willard's systems it **cannot prove successor totality** (pp. 3–4, 21) — it evades Pudlák by the same door — but the witness differs: a finite model of size `≤ 2⁰_p` (superexponentiation), built inside EA and relativised to the **superexponential cut**, where Willard's is a bounded valuation on a tableaux branch |
| **Beklemishev–Shamkanov** | the **structural rules** | **No.** See §5.2 |

An earlier draft's Pakhomov cell said `H_{<ω}` "proves every hereditarily finite
set exists; its `♯` is unbounded", making it a system that self-verifies
*without* growth restriction. That is a misreading of the source and is
withdrawn (`R1-review.md` §2.1). Correctly read, Pakhomov is not a
counterexample to the margin's necessity; he is a second *witness kind*, which is
why R1's criterion was generalised from naming rate to bounded semantic witness.

### 5.2 Beklemishev–Shamkanov's `S` is not self-verifying

Their contraction-free K4 breaks the **formalized** G2 argument — `⊢ ¬□⊥ ⟹ ⊢ ⊥`
fails — while Gödelian and Henkinian fixed points persist. The paper is explicit
that this does not produce a self-verifying system:

> "We remark that the system `S` does not provide a counterexample to the
> non-formalized version of G2, since `⇒ ¬□⊥` is not provable."
> — §6, p. 14 (image-verified 2026-09-02)

and, earlier in the same section:

> "Thus, we are still missing convincing examples of mathematical theories based
> on weak logics for which G2 would fail."
> — §6, p. 14 (image-verified 2026-09-02)

*Transcription note.* An earlier version of this passage expanded "G2", added
"in it", and called the second sentence the section's close. See
`R2-beklemishev-shamkanov-assessment.md` §1.

`S` is *consistent and does not assert its own consistency*, which is the
ordinary condition of a weak theory, not the SJAS condition. Listing it as a
third route to self-verification — as an earlier draft of this section did — was
an error of the same shape as the Pakhomov one: reading a paper's abstract result
as an instance of this stage's subject.

### 5.3 The corrected statement

The overclaim to avoid is the biconditional. **Self-verification is not "the
failure of G2's argument"** — `S` is precisely a case where the argument fails
and self-verification does not follow. Breaking the argument is *necessary*, not
sufficient: a theory must also actually **prove** `Con`, and that is an extra,
positive obligation on the theory's own axioms.

> **The corrected classification.** G2's argument is over-determined: it needs
> (i) a fixed point, (ii) the derivability conditions, (iii) the semantic step
> from an inconsistency-witness to falsity, and (iv) a structural licence to use
> a context twice. Each of (ii)–(iv) has been removed independently in the
> literature. Removing one makes self-verification *possible*; obtaining it
> additionally requires the theory to assert `Con` and stay consistent.
>
> - **Willard** removes (ii), by cost — and obtains a self-verifying arithmetic.
> - **Pakhomov** removes (iii), by finite models — and obtains a self-verifying
>   set theory.
> - **Beklemishev–Shamkanov** remove (iv), by dropping `□`-contraction — and
>   obtain a system with fixed points and no G2, but **not** a self-verifying
>   one; their own §6 says so.
> - (i) is free — Lawvere makes the fixed point available in any cartesian
>   closed setting — and nobody gives it up.

The margin is the right invariant for the first cell, and it is complete for that
cell, covering all five of Willard's dials at the strength §4 records. That is a
smaller result than R3 was set up to look for, and it is what the evidence
supports.

**Two successor questions, now better posed.**

1. *The empty cell.* Nobody has evaded G2 by attacking the fixed point itself.
   A setting whose internal structure is not cartesian closed — so the diagonal
   is genuinely unavailable rather than merely unusable — would be a fourth
   mechanism, and the first to make self-reference, rather than its cost, the
   thing that fails. Whether it can still be an arithmetic is open.
2. *Completing BS's cell.* Their route is the only one of the three that has
   **not** been carried to a self-verifying theory, and they name that as the
   open problem. Since `□`-contraction can hold in affine PA (§3 of their paper,
   obligation **RO1**), the target is not merely a contraction-free arithmetic
   but one in which the **restricted** rule fails and `Con` is still provable.

---

## 6. Does R3 hold? — the verdict

*Written 2026-09-02 after re-reading `Willard2011` §§4–5 as page images rather
than extracted text. Everything below is checked against the printed page.*

### What holds

**The definition holds.** `M(ξ) = inf_β {Log(q_β) − ♯(β)}` with the threshold
`M(ξ) ≥ 2` is exactly Willard's Definition 4.5, and Lemma 4.6 is exactly the
theorem that `M(ξ) ≥ 2` suffices for a consistent self-justifying system. The
margin is a legitimate object and the corpus already proves the thing R3 wanted
proved about it.

**Three of the five dials hold, and now with a proof rather than an argument.**
`♯` is defined by Defs. 4.1–4.4 without reference to the deduction method, so
any dial that varies only `d` moves `Log(q_β)` and cannot move `♯(β)`. That
covers `ℜ`, `Herb−k` and `Z`. This is the one-line lemma (§3.0) that the whole
of §3 needed and that none of the drafts had.

**The A-Stability ⟹ Tightness bridge holds and completes** (§2.2), including
the r.e.-versus-arbitrary gap, which closes by passing to the finite subset of
`β` the shortest refutation actually uses.

### What does not hold

**The Level(n) instance fails and is retracted** (§3.3). Group-3 is not part of
`B^ξ`; and because Level(n) varies a *sentence*, both terms move — at the
critical `β = B^ξ + Ψ`, Equation (20) makes them move in exact lockstep, so the
margin there is identically 1 regardless of level.

**The Hybrid(H) instance remains unplaced** (§3.1), for the same structural
reason: it varies the sentences, so both terms are in play.

**§3's original framing was not well-posed.** "Dial X lowers `Log(q_β)`" says
nothing about a *difference* unless the other term is checked. The drafts never
checked it. For the three apparatus dials the omission turned out to be
harmless — §3.0 shows the second term is fixed — but that was luck, not
argument, and for the two sentence dials it was fatal.

### What R3 claimed that it should not have

**"Generalising" Def. 4.5.** It is a restatement: `∀β. A(β) ≥ 2` and
`inf_β A(β) ≥ 2` say the same thing. Withdrawn in §2.

**Novelty for the stability bridge.** Willard already has A-stability alone as
sufficient for self-justification — Remark 5.2 announces it, Theorem 5.11 proves
it, Theorem 6.3 uses it. R3's route is *shorter*, going through Tightness and
Lemma 4.6 instead of 0-stability and Appendix C. A simplification is worth
having and is exactly this stage's remit; a new theorem it is not.

**Authorship of the `♯`-is-model-theoretic argument.** §3.1 presented it as the
Refinement's inference for withdrawing the Hybrid side-assignment. It is
Willard's own justification for Equation (20). Attributed in §2.1 and §3.1.

### What R3 missed, and is the best thing here

**The threshold 2 is forced, and the corpus says why.** When the
self-referential axiom is false, `Log(q) = ♯(B^ξ + Ψ) + 1` *exactly*
(Eqs. (19)–(20)). The fixed point sits at margin **exactly one**. Tightness's
`+2` is therefore precisely the demand that every inconsistent `β` have strictly
more margin than the fixed point achieves — and Lemma 4.6 is that one-line
contradiction. R3 carried the `2` as a given for three drafts. It is the content.

### Verdict

**R3 partly holds.** Its definition is sound and is Willard's; its bridge to
A-stability is sound and completable; three of its five dial instances are now
proved rather than argued. One instance is retracted, one remains open, and two
of its claims about its own novelty are withdrawn. Acceptance criterion **B5**
is **partly met at three of five**, not five of five as the previous revision
recorded.

The honest summary is that R3's *object* is right and its *method* was wrong:
it reasoned about one term of a difference. Fixing the method proved three cases
outright and broke one, which is a better outcome than the five hand-waved
cases it replaced.
