# Notes toward R4 — a native, non-arithmetic self-justifying model

*Working notes, 2026-09-07. **These are notes toward component R4, not R4.**
They answer a run of questions put during the R6 thread about what a
self-justifying *computational* model would have to look like, and each answer
is anchored where the corpus already settles the matter. Where the corpus does
not settle it, the note says so and opens an obligation.*

> **Status.** §§1, 2, 3 and 5 are anchored on image-verified corpus passages and
> carry the source's proof status at the point of use. §4 is a research note with
> nothing verified. §6's list distinguishes held witnesses from citations
> retrieved by search and not read.

---

## 1. The bound of a bounded quantifier is a term — and it is the same `t`

The question: in "a *term* `t` is …" and "`∀v ≤ t Ψ(v)`", are both `t`s terms?

**Yes, and Willard uses the same letter, defining it in the sentence
immediately before.** `Willard2005` printed p. 5, image-verified:

> "Here, a *term* `t` is defined to be a constant, variable or a U-Grounding
> function symbol (whose input arguments are recursively defined terms). Also,
> the quantifiers in the wffs `∀ v ≤ t Ψ(v)` and `∃ v ≤ t Ψ(v)` are called
> *bounded quantifiers*."

Independent confirmation, `Willard2011` Definition D.1(i), printed p. 38,
image-verified: `Δ*₀` "is defined to represent the set of all formulae in `L*`'s
language, whose **quantifiers are bounded in an arbitrary manner by terms
employing the U-Grounding function symbols**".

And the reason it matters is stated on the same 2005 page:

> "The use of logic's conventional notation about `Π_n` and `Σ_m` sentences is
> technically inappropriate in this paper because the latter notation (with its
> multiplication function symbol) is suitable only for axiom systems which
> recognize multiplication as a total function."

So `Δ*₀` is **not a fixed class**. It is indexed by the term signature, and
changing the signature changes it — which is why `Π*_n`/`Σ*_n` exist at all.

**Terms occupy two positions, not one**, and the signature is upstream of both.
The grammar is three-layered, and both papers state the middle layer:
`Willard2005` p. 5 — a `Δ*₀` formula "uses the U-Grounding primitives as its
**function symbols**, the two **relation symbols** of "=" and "<", and all its
quantifiers are bounded"; `Willard2011` Definition D.1 — `L*` is built from "the
eight U-grounding function operations, the usual atomic predicate symbols of "="
and "≤", and the three constant symbols `K₀, K₁` and `K₂`".

| layer | built from |
| --- | --- |
| terms | constants, variables, U-Grounding function symbols applied to terms |
| atomic formulas | `=` and `<`/`≤` applied to **terms** |
| formulas | atomic formulas, connectives, and quantifiers whose bounds are **terms** |

**Consequence, stated more strongly than an earlier draft of this section did.**
It is not only that a relation cannot be a bound. A language with no
multiplication function symbol cannot write `x·y` **anywhere** — not as a bound
and not inside an atomic formula, so `x·y = z` is not formable either. That is
why `Willard2005` Eq. (4) has the shape it has: with no `·` available, the graph
of multiplication must be written from function symbols that do exist, which is
what `z/x = y ∧ (z−1)/x < y` does. `M(x,y,z)` is then a *formula*, and formulas
cannot occupy a bound position, so `∃w ≤ M(x,y,z)` is no more formable than
`∃w ≤ x·y`. `code/tabt` now carries a recogniser for the term layer — `ugrounding_term`, with a policy flag defaulting to the file's previous open behaviour.

*Two signature differences found while stating this, both recorded in the
Codification drift ledger and neither load-bearing:* **D83**, `Willard2005` lists
`<` as primitive where `Willard2011` Definition D.1 lists `≤`; **D84**, the 2005
page writes its bounded quantifiers with `≤` two sentences before listing `<` as
primitive, which a checker must decide between — `code/tabt` currently reads `≤`
as primitive, carrying it as a binary `leq` formula constructor.

## 2. Admitting a new function symbol: a static side condition, and Willard states it

The question: is the admission rule a `define` form, or a runtime primitive?

**Neither a runtime primitive nor an unchecked `define`: a definitional
extension carrying a discharge obligation, checked statically at definition
time. And the corpus already gives the criterion.** `Willard1993-TR`
**Remark 6**, printed p. 34, proof status **`stated-only`**: `G₀`'s definition
"was **quite arbitrary**, and our main theorems trivially extend for more
general definitions". Call `f̂[i,j]` **slowly growing** iff

`f̂[i,j](x̄) ≤ i · 2^j · Max(x̄)`.

Then **Propositions 1–4, 8 and 9 hold when any set of slowly growing functions
is added to `G₀`**, and Proposition 7 when any non-growth set is added to `G⁻`.
The invariant is the growth class, not the signature.

Three things follow for a language design.

- **The obligation is a growth bound, not a totality proof.** To add `f`, exhibit
  constants `i, j` with `f(x̄) ≤ i·2^j·Max(x̄)`. `Scalar_k` discharges it
  trivially — `k·x` *is* the term `x+x+…+x` — and the chapter's
  `Andmultiply(x,y,z)`, "multiply then mask with `z`", discharges it by the mask
  (drift **D10**). Unrestricted `x·y` cannot.
- **The check is decidable for the common case.** If `f` is given by a term over
  the existing signature, the bound is computed by induction: every U-Grounding
  term satisfies `t(x̄) ≤ 2^{a_t}·Max(x̄)` with `a_t` the number of
  Addition/Double occurrences *in the term* (derivation in
  [`VERIFICATION.md`](VERIFICATION.md) **Computed claims**). That derivation was
  made independently of Remark 6 and lands on the same bound, which is a useful
  cross-check on both.
- **The licence is partial and its scope must be carried.** Remark 6 is
  `stated-only`, and it covers Propositions 1–4, 8 and 9 — **not** Propositions
  5, 6, or Solovay's Theorem. A `define` form built on it inherits that scope.

## 3. Withdrawn: SJAS as a *decidable* index language

An earlier suggestion in this thread was that a DML-style index language
extended with `Log`, `Root`, `Count`, `Max` and variable `Division` would "stay
inside Willard's growth budget while leaving Presburger decidability", and that
this was an interesting unoccupied design point.

**The framing is withdrawn.** Willard's `Δ₀` formulas encode Turing machines:
`Willard1993-TR` printed p. 18 (imaged) lists as an immediate consequence of
Lemma 5.1 that "There exists a `Δ₀` formula `Turing(x,y,z)` which states `x` is
an encoding of the first `y` states of a Turing Machine whose initial state is
`z`", and **Lemma 5.4** (`sketch`) builds `Prf2_B(x,y₁,y₂)` in which `y₂`
"describes the successive states of a Turing Machine verifying that `y₁` is well
defined". A theory whose bounded formulas express Turing-machine computation is
not a decidable constraint domain.

So the two restrictions do **not** compose into a sweet spot. DML restricts its
index language for **decidability of constraint solving**; Willard restricts his
signature for **growth**. They agree at the multiplication line by coincidence of
consequence, not of purpose, and Willard's system is undecidable regardless. Any
SJAS-as-index-language design accepts an undecidable index domain and gives up
the DML premise.

What survives of the observation is narrower and still worth stating: **the
growth criterion and the decidability criterion are independent axes**, and
Remark 6's `i·2^j·Max(x̄)` is a criterion a refinement-type designer could adopt
for a *different* purpose than Willard's, without inheriting his undecidability
— provided the index language stays decidable on its own terms.

## 4. Research note: self-referential refinement-type systems

**Obligation RO3, opened 2026-09-07.** Nothing verified; no witness held.

The question is whether any DML-style or refinement-type system has been given a
*self-referential* index theory — one that reasons about its own typechecking,
rather than only discharging constraints. Willard's Group-2 (reflection over a
base theory's `Π₁` theorems) and Group-3 (self-consistency) have no counterpart
in any refinement system known to this stage. Targets to check: DML (Xi and
Pfenning), Liquid Types, F*, and the self-verification literature around
proof-carrying code. The payoff would be a setting where the index theory's own
consistency is available to the elaborator — which is the Gödel-machine shape.

## 5. What `Tab` costs operationally, and the subset that gets modus ponens back

The question: what is the operational effect of `□_Tab` not being closed under
modus ponens, and what if one works only in the subset the system internally
recognises as self-consistent?

**The cost is proof length, and it is Willard's Linear-Sum Effect.**
`Willard2020` printed p. 279, image-verified: modus ponens "assures that a proof
of a theorem `ψ` … has a length **no greater than the sum** of the proof-lengths
needed to derive `φ` and `φ → ψ`", and "This 'Linear-Sum Effect' does not apply,
actually, also to *Tab*-deduction because it owns no analog of a modus ponens
rule". So in `Tab`, chaining lemmas is not length-additive: each composition is
either performed outside the system — `Willard1993-TR` **Remark 2**'s declared
Meta-Logic convention — or paid for in a proof with no linear-sum guarantee.

**And the second half of the question is Willard's actual design dial.**
`Willard2020` §7 defines a **`Z`-Enriched** tableau proof as one additionally
admitting `Υ ∨ ¬Υ` as a logical axiom for `Υ ∈ Z`; the extraction records that
"**`Z` is the right dial because it is the set of licensed modus-ponens middle
terms**". The boundary, **stated without proof** at printed p. 19:

| `Z` | evasion |
| --- | --- |
| `Δ*₀` | **survives** |
| `Π*₁` | **conjectured** to survive — "this fact has not yet been formally proven" |
| `Π*₂` and above | **fails** |

So: working in a restricted subset does not merely preserve self-consistency, it
**buys modus ponens back on that subset**, and the corpus says how far the subset
can be widened. `Δ*₀` is safe; `Π*₁` is the open problem — and it is the same
`Π₁` target [`RO2-contraction-growth-rate.md`](RO2-contraction-growth-rate.md)
§4 names as the primary objective. Footnote 7 gives the reason for failure above
it: "the sharp compression in proof lengths produces Gödel-like Diagonalization
compressions".

## 6. Light, soft, bounded and affine logics — what is relevant, and what is held

For concretising B–S and for a native non-arithmetic SJAS. **Held** first.

| System | Reference | Why relevant | Status |
| --- | --- | --- | --- |
| **Light Affine Logic** | Asperti and Roversi, [arXiv:cs/0006010](https://arxiv.org/abs/cs/0006010) | **Affine**: keeps weakening, drops contraction — the B–S structural setting exactly. "Polynomially costing cut elimination" | **held**, `lit/` |
| **Bounded Linear Logic** | Dal Lago and Hofmann, LMCS 6(4:7) 2010, [arXiv:0904.2675](https://arxiv.org/abs/0904.2675) | The accessible route to BLL's **indexed bang** `!_{x<p}A` — a `□` carrying its resource bound as a term | **held**, `lit/` |
| BLL, original | Girard, Scedrov and Scott, TCS 97(1) (1992) 1–66, DOI 10.1016/0304-3975(92)90386-T | The source of the indexed bang | **not held** |
| Light Linear Logic | Girard, Inf. and Comp. 143(2) (1998) 175–204, DOI 10.1006/inco.1998.2700 | The light bang; polytime cut elimination | **not held** |
| Soft Linear Logic | Lafont, TCS 318 (2004) 163–180, DOI 10.1016/j.tcs.2003.10.018 | A simpler polytime discipline than LLL | **not held** |
| Elementary Linear Logic | — | Elementary-time discipline; the weaker sibling of LLL | **not held, not verified** |
| Contraction-free arithmetic | Restall, *An Introduction to Substructural Logics*, ch. 11 | Cited by B–S §6 for the system whose `□`-contraction is admissible "which … still yields G2" | **not held** |
| Contraction-free set theory | Grishin, cited as B–S [7, 8] | B–S's own analogy for contraction's role in Liar-type paradoxes | **not held** |

The four "not held" logic entries are recorded from search results with venue
and DOI and have **not been read**; the two held ones have been identified from
their own first pages. Nothing in this stage should cite the unheld four for
content.

**Which is the right one to start from.** Light *Affine* Logic, because B–S's
setting keeps weakening and drops contraction, and LAL is the system built on
exactly that pair. BLL's indexed bang is the right shape for the second step —
the `□` that carries a term bound — and Dal Lago–Hofmann is the held route to
it.
