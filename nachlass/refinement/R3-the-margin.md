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
| `q_β` | the shortest proof of `0=1` from `β` (Def. 4.5) |

`♯(Υ)` is the **envelope**: how far out `Υ` remains true when its quantifiers are
cut down to `2^N`. `Log(q_β)` is the **cost**.

Willard's Definition 4.5 puts them in one inequality. A configuration `ξ` is
**Tight** iff every inconsistent `β ⊃ B^ξ` of `Π^ξ₁` sentences satisfies

> **`Log(q_β) ≥ ♯(β) + 2`**

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
- **"Shortest" means least Gödel number.** Every consistency-preservation proof
  in the corpus is a minimal-counterexample argument on exactly that quantity —
  "take `P` minimising `Θ(P)`" — and constructs a smaller `R` with
  `Θ(R) < Θ(P)` (codified §6.9).

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

**Relation to the engine.** `Willard2011`'s preservation theorem (Thm 5.9,
`full`) consumes **EA-stability**, whose definition an earlier draft of this
paragraph got wrong in three ways. Correctly (Defs. 5.1, 5.3, 5.5):

- an **R-View** `θ` is *any* r.e. set of `Π^ξ₁` sentences — Remark 5.2 stresses
  that it is **not required to be true** — and `RE-Class(ξ)` is the set of all of
  them;
- `ξ` is **A-Stable** iff for every `θ ∈ RE-Class(ξ)`: if `Υ` is a `Π^ξ₁` theorem
  of `θ ∪ B^ξ` via a proof `p` with `Log(p) ≤ ♯(θ)+1`, then `Υ` is `Good{½♯(θ)}`
  (Def. **5.1**);
- `ξ` is **E-Stable** iff the same for `Σ^ξ₁` theorems, with `Good{½⌊Log(p)⌋−1}`
  (Def. **5.3**);
- `ξ` is **EA-stable** iff it is **both** (Def. **5.5**), and Thm 5.9's
  dependencies are Defs. 5.5 and 5.7 — not 5.1.

The earlier draft gave only the `Π` half, attributed it to the wrong definition
number, and dropped the universal quantification over possibly-false R-Views,
which is the clause doing the work.

Stability bounds what a *short proof can reach*; Tightness says a *refutation
cannot be short*. The bridge **Stability ⟹ Tightness** is closer than that draft
allowed: instantiating A-Stability at `Υ = (0{=}1)` and `θ = β`, a refutation
with `Log(q_β) ≤ ♯(β)+1` would make `0=1` `Good{½♯(β)}`, which it is not. That is
a sketch, not a proof, and it is offered as such. Note also that `Willard2011`
**Lemma 4.6** is itself a Tightness-based engine result — if `ξ` is tight then
`B^ξ + SelfRef(B^ξ,d)` is a consistent self-justifying system — so Tightness is
not merely descriptive.

---

## 3. The claim: every dial moves one of the two terms

> **Claim.** Each of the corpus's five boundary dials varies either `♯(β)` or
> `Log(q_β)`, and its transition point is where `M(ξ)` crosses `2`. The dials are
> not five phenomena; they are five ways of writing on the same inequality.

Four of the five push the **cost** term `Log(q_β)` down. The fifth — the naming
dial — moves the margin by a route this document does not settle; §3.1 says why,
and the two-families split an earlier draft asserted is not sustained.

### 3.1 The naming dial — side undetermined

**Hybrid(H)** (`Willard2006a` p. 30). `C_i = ⌈2^{[Log i]^H}⌉·C_{i−1}`, so

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
argument was given, and there is a difficulty.** `♯` is purely model-theoretic
(`Good(N)` = `Scope_E(Υ,N)` true in the standard model), and the naming axioms
are *true* sentences, so they are `Good(∞)` and cannot themselves lower `♯(β)`.
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
| **`ℜ`** (`Willard2004`) | a `Tab-ℜ-List` proof may use intermediate theorems from `ℜ` | on `ℜ`, composition becomes **linear-sum** rather than super-exponential (`Willard2002a` Thm 2.2), so `q_β` collapses |
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

### 3.3 The level dial — lowers `Log(q_β)` via the base

**Level(n)** (`Willard2004` §§1–2). An earlier draft said this dial "does not
change what proofs cost; it changes **what counts as a refutation**" — and then
concluded that `q_β` falls. Those are inconsistent: under the Definition of §2,
`q_β` is the least Gödel number of a proof of the *fixed* target `0=1`, so a dial
that changed what counts as a refutation would change something the margin does
not measure.

The coherent mechanism is simpler and does move the cost term. Level(n) fixes
what the **Group-3 axiom asserts**, and Group-3 is part of `B^ξ`. Raising `n`
strengthens that axiom, hence strengthens the base, hence makes `0=1` **cheaper
to derive** from an inconsistent `β ⊃ B^ξ`: a stronger false self-referential
axiom is a better lever. `Log(q_β)` falls and the margin closes.

The Willard quote an earlier draft adduced — that all the level definitions "are
equivalent to each other under strong enough models" — does **not** support
this. It says the levels are extensionally the same in strong models, which is a
statement about their equivalence, not about refutation length. It is withdrawn
as evidence; the mechanism above is offered as this Refinement's reading, and it
is the least well-supported of the five.

### 3.4 Summary

| Dial | Term moved | Safe | Fatal |
| --- | --- | --- | --- |
| Hybrid(H) | **undetermined** (§3.1) | `H = 1` | `H > 1` |
| `ℜ` | `Log(q_β)` ↓ | `Π*₁ ∪ Σ*₁` | `Π*₂`, `Σ*₂` |
| `Herb−k` | `Log(q_β)` ↓ | `k = 1` | `k = 2` |
| `Z` | `Log(q_β)` ↓ | `Δ*₀` | `Π*₂` and above |
| Level(n) | `Log(q_β)` ↓ (via assertion strength) | Level(1) | Level(2+) |

**This answers a question the Codification left open.** Drift **D38** records
five parameterisations "none of which Willard relates to the others" and asks
for a single presentation. The margin supplies one: **one inequality, two terms,
five ways of writing on it**.

It does *not* explain why Hybrid(H) looks different from the other four — an
earlier draft claimed it did, by putting Hybrid alone on the envelope side, and
§3.1 withdraws that. Four dials are shown to lower `Log(q_β)`; the naming dial
is shown to move the margin without the term being identified. D38's request is
answered for four of five, and reduced to a sharper question for the fifth.

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

**Inherited weakness.** Per `codified-sjas.md` §8.1, exactly **one** of the five
dials is `full` on both halves:

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
