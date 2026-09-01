# Extraction: Willard2011

> Spine extraction (component C6). Template: charter ADR-0001 §Decision.
> **Anchoring convention**: this witness's printed page number is **PDF page
> − 1** (verified at PDF 17 = printed 16, PDF 33 = printed 32, PDF 40 =
> printed 39). Anchors below are **printed** pages. Text-layer quality is good
> (LaTeX-generated), but superscripts are lost — `Π^ξ_1` renders as `Πξ1`,
> `Γ^k` as `Γk` — so all load-bearing statements were read from page images.

## 1. Identity and witness

| Field | Value |
| --- | --- |
| Corpus key | `Willard2011` |
| Title | A Detailed Examination of Methods for Unifying, Simplifying and Extending Several Results About Self-Justifying Logics |
| Venue | arXiv 1108.6330, **v8, 31 Dec 2011** (never journal-published; see §7) |
| Witness | `nachlass/papers/willard2011_self_justifying_logics_arxiv_1108.6330.pdf` (64 pp.) |
| Text aid | `../sources-text/willard2011.txt` (27,813 words) |
| Funding | NSF Grant CCR 0956495 |
| Structure | §1 Introduction (1–7); §2 Literature Survey (3–7); §3 Generic Configurations (7–12); §4 Five Helpful Definitions and An Informative Lemma (12–16); §5 The First Two Meta-Theorems about Self-Justification (15–20); §6 Four Further Meta-Theorems (20–30); §7 Concluding Remarks (30–31); Appendix A (32–33); B (33–35); C (35–37); D (37–50); E (50–52); F (52–55); G (55–59); References (60–64) |

## 2. Role in corpus — the claim to be audited

The title states three claims: **unifying**, **simplifying**, **extending**.
Willard makes the unification claim precisely (printed p. 3):

> The proofs in our prior papers were challenging primarily because they
> required one to separate the local combinatorial methods employed in
> [59, 64, 66, 68]'s particular applications from the common principles that
> underlied behind all these works. **Our Theorems 5.9, 5.11 and 6.6 will
> rectify this problem by identifying common components that unite these four
> paradigms.** (Theorems 6.3, 6.10, 6.12, E.1, G.2 and G.3 will then carry on
> in further directions.)

Resolved against the bibliography: **[59] = `Willard1993`**, **[64] =
`Willard2005`**, **[66] = `Willard2006a`**, **[68] = `Willard2009`**. The audit
of these claims is the separate C6 deliverable
[`willard2011-subsumption-audit.md`](willard2011-subsumption-audit.md).

The abstract's own headline is different from the unification: "Its perhaps
single most surprising new result will be its exploration of a **viable
alternative to conventional reflection principles**" — i.e. Theorem 6.12
(§3.5).

## 3. Systems and machinery defined

### 3.1 The vocabulary shift

| 2011 term | Definition | Corresponds to |
| --- | --- | --- |
| **Self Justifying** (of the *pair* `(α,d)`) | (i) one of α's theorems states d applied to α yields a consistent set; (ii) α is in fact consistent | **`Willard2005`'s "Introspectively Unified Logic"** — *not* its "Self-Justifying", which was a property of α alone (∃d). Drift D18 |
| `SelfRef(α,d)` | the "no proof of 0=1 from α + this sentence" axiom | 1993's Group-3 sentence; 2005's Eq. (6) |
| **Type-M / Type-A / Type-S / Type-NS** | contains axioms (1)+(2)+(3) / (1)+(2) / (1) only / none, where (1) successor-total, (2) addition-total, (3) multiplication-total | The 2011 taxonomy of the growth axis; supersedes 1993's informal "three categories" (printed p. 3) |
| **Type-Almost-M** | proves (4) `∀x∀y∃z x+y=z` and (5) `∀x∀y∃z x∗y=z` as *theorems* while treating none of (1)–(5) as axioms | New in 2011; captures systems using function symbols for `+`, `∗` |

### 3.2 Generic Configurations — the unification device (§3)

**Definition 3.4.** A *Generic Configuration* ξ is a **5-tuple**
`(L^ξ, Δ^ξ_0, B^ξ, d, g)`:

1. `L^ξ` — language with `0`, `1`, `2`, `=`, `≤`, `Maximum(x,y)`, and enough
   function/constant symbols that every integer `k` has a term `T_k`;
2. `Δ^ξ_0` — fully-bounded formulae, closed under negation, rich enough to
   contain `Add(x,y,z)` and `Mult(x,y,z)` as the *graphs* of addition and
   multiplication (Definition 3.1 then builds `Π^ξ_n`/`Σ^ξ_n` canonically);
3. `B^ξ` — a **Base Axiom System**, true in Standard-M and **Σ^ξ_1 complete**
   (proves every true `Σ^ξ_1` sentence, refutes every false `Π^ξ_1` one);
4. `d` — deduction method, "sufficiently conventional" to satisfy the
   indirect-implication property of Gödel's Completeness Theorem (footnote 5:
   with or without a built-in modus ponens rule, it proves `Z` from `X`, `Y`
   and `(X ∧ Y) → Z`, possibly at length);
5. `g` — **a method for encoding the Gödel numbers of proofs**.

This is the paper's central abstraction: each earlier paper becomes one choice
of ξ. Note that the *coding method is now a parameter*, which is what lets
Appendix D state a coding **condition** rather than exhibit a construction
(§3.6).

### 3.3 The measure, and "Tight" (§4)

- **Definition 4.1** `E(n)` — a term of `L^ξ` denoting `2^n` (any reasonable
  method; no exponent function symbol is presumed).
- **Definition 4.2** `Scope_E(Υ,N)` — Υ with every *unbounded* quantifier
  rebounded by `E(N)`; bounded quantifiers untouched. Purely metatheoretic:
  "will actually not be used in our physical encodings of proofs".
- **Definition 4.3** Υ is **Good(N)** iff `Scope_E(Υ,N)` is true in Standard-M.
  Footnote 6 records the polarity flip: Good(N) is *weaker* than Good(∞) for
  `Π^ξ_1` sentences and *stronger* for `Σ^ξ_1`.
- **Definition 4.4** `♯(Υ)` — the largest `J` with Υ Good(J) (∞ if all).
- **Definition 4.5** ξ is **Tight** iff every inconsistent `β ⊃ B^ξ` of
  `Π^ξ_1` sentences, with `q_β` the shortest proof of `0=1`, satisfies
  **`Log(q_β) ≥ ♯(β) + 2`**.

Together these replace `Willard2005`'s `App∀(a)`/`App∃(b)` envelope pair with
a single measure `♯` on the log scale.

### 3.4 The stability hierarchy and the two meta-theorems (§5)

An **R-View** θ is any r.e. set of `Π^ξ_1` sentences — *not required to be
true*; `RE-Class(ξ)` is the set of all of them.

| Property | Condition on every θ ∈ RE-Class(ξ) | Anchor |
| --- | --- | --- |
| **A-Stable** (∗) | if Υ is a `Π^ξ_1` theorem of `θ ∪ B^ξ` via proof `p` with `Log(p) ≤ ♯(θ)+1`, then Υ satisfies **Good{½♯(θ)}** | Def. 5.1, p. 16 |
| **E-Stable** (∗∗) | if Υ is a `Σ^ξ_1` theorem via such a `p`, then Υ satisfies **Good{½⌊Log(p)⌋ − 1}** (which implies Good{½♯(θ)}) | Def. 5.3, p. 16 |
| **EA-Stable** | both | Def. 5.5, p. 17 |
| **0-Stable** (∗∗∗) | if Υ is a `Δ^ξ_0` theorem via such a `p`, then Υ is true in Standard-M. "strictly weaker" than ∗ and ∗∗ | Def. 5.10, p. 19 |

- **Definition 5.6** `Level(k^ξ) Consistent`: no proofs from α of both a
  `Π^ξ_k` sentence and the `Σ^ξ_k` sentence that is its negation.
- **Definition 5.7** `SelfCons_k(β,d)` — the self-referencing `Π^ξ_1` sentence
  declaring `β + SelfCons_k(β,d)`'s Level(k^ξ) consistency. **This is the
  origin of the `SelfCons1` name used throughout the Proflog line.**
  Remark 5.8: for `k ≥ 2` it is typically too strong; even at `k = 1` it is
  significantly stronger than `SelfRef`, which only denies a proof of `0=1`.
- **Theorem 5.9** ξ EA-stable ⇒ `B^ξ + SelfCons_1(B^ξ,d)` is self-justifying.
- **Theorem 5.11** ξ 0-stable ⇒ `B^ξ + SelfCons_0(B^ξ,d)` is self-justifying
  (Appendix C extends this to E-stable and A-stable).

### 3.5 Consistency preservation, Group-2, and the reflection result (§6)

- **Lemma 6.1** adding any r.e. set θ• of *true* `Π^ξ_1` sentences to the base
  preserves 0-/A-/E-/EA-stability (four claims; Appendix B proves only claim
  (i) in detail, "because all four claims have similar proofs").
- **Definition 6.2** `G^ξ_k(θ) = θ ∪ B^ξ ∪ SelfCons_k{[θ ∪ B^ξ], d}`, and
  `G^ξ_k` is **Consistency Preserving** iff `G^ξ_k(θ)` is consistent whenever
  every sentence of θ is true in Standard-M. *(This is `Willard2005`
  Definition 1, generalized.)*
- **Theorem 6.3** `G^ξ_1` is consistency-preserving when ξ is EA-stable;
  `G^ξ_0` when ξ is A-stable, E-stable **or** 0-stable.
- **Definition 6.5** the **Group-2 Schema** for `(B,D)`: one axiom
  `∀q {Prf^D_B(⌜Ψ⌝,q) → Ψ}` per `Π^ξ_1` sentence Ψ — "so as to keep our
  terminology consistent with **[59, 61, 64, 66]**'s notation", i.e. the same
  Group-2 as 1993/2001/2005/2006a.
- **Theorem 6.6** for any ξ and any `(B,D)` whose `Π^ξ_1` theorems are true:
  an r.e. self-justifying system exists proving all of them and recognizing
  its own Level(1^ξ) consistency (EA-stable) or Level(0^ξ) (A/E/0-stable).
- **Global Simulation Sentence** (Eq. 28): a **single finite axiom**
  `∀t∀q∀x {[Prf^D_B(t,q) ∧ Check^ξ(t)] → Test^ξ(t,x)}` that simulates the
  infinite Group-2 schema, where `Test^ξ` satisfies `Ψ ↔ ∀x Test^ξ(⌜Ψ⌝,x)`.
  Example 6.7 gives `Test^ξ_0(t,x) =def ¬NegPrf^ξ(t,x)`, valid because `B^ξ`
  is `Σ^ξ_1` complete. Definition 6.8 indexes the list.
- **Remark 6.9** neither Group-2 nor GlobSim dominates: GlobSim is finite, but
  `B^ξ + GlobSim` typically proves only `∀x Test^ξ_j(⌜Ψ⌝,x)` — equivalent to
  Ψ in Standard-M but not provably so (footnote 13: the difficulty is `Π^ξ_1`
  theorems with long bounded-quantifier prefixes).
- **Theorem 6.10** for each `j` a *finitized* extension `β_j` of `B^ξ` exists
  containing `GlobSim^D_B(ξ,j)` and recognizing Level(1^ξ) (or Level(0^ξ)).
- **Theorem 6.12 — the Translational Reflection Principle.** Löb's Theorem
  (and [61]'s Theorem 7.2 for weak systems) blocks
  `Reflect_{α,d}(Ψ) ≡ ∀p[Prf_{α,d}(⌜Ψ⌝,p) ⇒ Ψ]` for all `Π^ξ_1` Ψ
  simultaneously. But with `T` a translation mapping Ψ to a Standard-M-
  equivalent `Ψ^T` **that is itself `Π^ξ_1`**, an EA-stable ξ's
  `α = B^ξ + SelfCons_1(B^ξ)` **can** prove
  `∀p[Prf_{α,d}(⌜Ψ⌝,p) ⇒ Ψ^T]` (Eq. 34) for all its `Π^ξ_1` sentences.
  Proof: take `Ψ^T = ∀x Test^ξ_0(⌜Ψ⌝,x)`; a proof of Ψ plus `SelfCons_1`
  gives the non-existence of a proof of `¬Ψ`, which *is* `Ψ^T`.
  Footnote 15 gives the evasion mechanism: Ψ and `Ψ^T` are equivalent in
  Standard-M **but not from α's perspective**, so the diagonal contradiction
  does not arise and α cannot prove the Gödel sentence `✵`.

### 3.6 The four sample configurations (Appendix D)

| ξ | Language / type | Deduction `d` | Stability | Level | Prior paper |
| --- | --- | --- | --- | --- | --- |
| **ξ\*** | U-Grounding eight; **Type-A** (addition total) | semantic tableaux | EA-stable (Theorem D.4) | 1 | "closest analog … appeared in **Section 5 of [64]**" = `Willard2005` §5 |
| **ξ\*\*** | identical to ξ\* | **Tab−U\*₁** (modus ponens on `Π*₁`/`Σ*₁`) | EA-stable | 1 | `Willard2005` §5.3; "The formal definition of Tab−U\*₁ deduction had appeared in [64]. It will be unnecessary to repeat here" |
| **ξ⁻** | `L⁻`: the six Grounding functions only — **no addition or doubling**; `Add`/`Mult` as 3-way predicates; base uses constants `K_i` with `Add(K_i,K_i,K_{i+1})` (Eq. 49) | **Hilbert, unrestricted modus ponens** | EA-stable | 1 | "represents **a new way of proving [66]'s Theorem 3**" = `Willard2006a` |
| **ξ^R** | `Δ^R_0` class (bounded quantifiers use only `Maximum`); the `Ax-3` axiomatization of `IΣ₀` | **Herbrand-style** (Skolemize, then propositional refutation) | E-stable | 0 | `Willard2009`, replying to Kolodziejczyk's 2005 email |

**Definition D.1** fixes ξ\*: `L*` over the eight U-Grounding operations (the
six non-growth Grounding functions — Subtraction, Division, `Root`, `Maximum`,
`Logarithm`, `Count` — plus addition and `Double`), `=`, `≤`, constants
`K_0,K_1,K_2`; `Δ*_0` = all `L*` formulae with bounded quantifiers; `B*` any
consistent set of `Π*_1` sentences proving every valid `Δ*_0` sentence **plus**

```
∀x ∀y ∃z ≤ x+y : { z = x+y }                                        (46)
```

described as a "very precise **Π\*₁ styled** declaration that addition is a
total function". *This is the mechanism behind composition obligation O11*:
the totality axiom is `Π*₁` rather than `Π₂` exactly because the function
symbol `+` makes the bound `x+y` expressible.

**The coding condition** (Definition D.1 part iv + footnote 23, printed p. 39):

> The **Conventional Tableaux Encoding Criteria** requires that the Gödel
> number of a semantic tableaux proof, with `J` function symbols, must be at
> least as large as **32^J**. It is clear that all the usual methods for
> generating the Gödel codes satisfy this criteria. This is because any proof
> that has `J` function symbols will contain at least **2J logical symbols**
> and thus employ at least **5J bits**.

with "The Appendix A of [64] provides one example of a possible tableaux
encoding method. **Any other natural mechanism for encoding tableaux proofs is
equally suitable.**" This is the promotion of a *construction* (1993 Appendix
A; 2005 Appendix A) to a *condition* — the coding axis of the unification.

The tableau rules 1–8 recited on printed p. 40 are `Willard2005` §2's eight
rules verbatim; `Reverse(Φ)` is 2005's "¬Φ rewritten in Prenex\* form".

**Remark D.5**: "the declaration that multiplication is a total function is the
**trigger-point** causing the semantic tableaux version of the Second
Incompleteness Theorem to become active."

**The tight-fit boundary** (printed p. 47, Items 1–4): Theorem 2.1 forbids
growth functions in `B⁻`; [66] shows replacing Eq. (49)'s additive constant
schema with the multiplicative `Mult(C_i,C_i,C_{i+1})` (Eq. 50) collapses ξ⁻
— "a generalization of the Second Incompleteness Theorem implies the
modification … is **not even 0-stable**", *stated without proof* ("There is no
space to prove it here"); [62,67] show adding multiplication-totality to `B*`
collapses both tableaux evasions; [63] (= `Willard2004`) shows extending
ξ\*\*'s modus ponens to `Π*₂` or `Σ*₂` collapses it.

### 3.7 Appendix A — the fixed point

Notation `Neg^k`, `Prf^d_β`, `ExPrf^d_β`, `Subst`, `SubstPrf^d_β` — the same
five constructs as `Willard1993-TR` Appendix A and `Willard2005` §3. Then

```
SubstPrf^d_β(g,t,p)  ≡  Prf^d_β(t,p) ∨ ∃h ≤ p { Subst(g,h) ∧ ExPrf^d_β(h,t,p) }   (35)
Γ^k(g) ≡ ∀x∀y∀p∀q ¬{ Neg^k(x,y) ∧ SubstPrf^d_β(g,x,p) ∧ SubstPrf^d_β(g,y,q) }     (36)
SelfCons_k(β,d) = Γ^k(n̄),  n̄ = Γ(g)'s Gödel number                              (37)
```

Equation (35) is **`Willard2005` Eq. (7) verbatim** (with `Δ^ξ_0` for `Δ*_0`);
Equation (36) is **2005 Eq. (8)** with `Neg^k` for `Pair` and the level
parameter `k` added. The `Γ_1(g)` skeleton implemented in the Proflog line
(ADR-0065/0069/0087) is this construction at `k = 1`.

### 3.8 Appendix G — finite axiomatization done properly

**Definition G.1** Ψ is `Braced^ξ(Φ,j)` when `B^ξ + Φ` proves
`{∀x Test^ξ_j(⌜Ψ⌝,x)} → Ψ` (Eq. 53) — i.e. the *converse* of the Test
equivalence is available, upgrading simulated knowledge to genuine knowledge.

**Theorem G.2** builds `β_j` = `B^ξ` plus **exactly three added sentences**:
(1) the `Π^ξ_1` sentence Φ; (2) `GlobSim^D_B(ξ,j)`; (3)
`SelfCons_k{[θ ∪ B^ξ], d}` with `k = 1` or `0`. It proves every
`Braced^ξ(Φ,j)` theorem of `(B,D)` **in the full sense** and recognizes its own
Level(k^ξ) consistency.

**Theorem G.3** For any of ξ\*, ξ\*\*, ξ⁻, ξ^R and **any `c > 0`**, `β_j` can
be arranged to prove **all** of `(B,D)`'s `Size^ξ(c)` `Π^ξ_1` theorems — those
with at most `c` quantifiers, bounded and unbounded — while recognizing
Level(1) consistency (ξ\*, ξ\*\*, ξ⁻) or Level(0) (ξ^R). *Proof Sketch.*

## 4. Numbered-item inventory

Verification: `V` = read from the page image; `T` = text layer only.

| Label | Page | Digest | Proof | Ver. |
| --- | --- | --- | --- | --- |
| Theorem 2.1 | 4 | **Solovay's 1994 generalization** of Pudlák's 1985 theorem using Nelson and Wilkie–Paris: no α containing the Type-S axiom (with `x'≠0`, `x'=y' ⇔ x=y`) can recognize its own Hilbert consistency while treating `+`,`∗` as 3-way relations with the usual identity/associative/commutative/distributive properties | **cited** — "Solovay never published any precise proof … which he privately communicated [44] to us" | T |
| Definition 3.1 | 7 | `Δ^ξ_0` closed under negation; `Π^ξ_n`/`Σ^ξ_n` built canonically | n/a | T |
| Example 3.2 | 8 | `Δ^A_0` (textbook `Δ_0`) versus `Δ^R_0` (bounded quantifiers may use only `Maximum`) | n/a | T |
| Definition 3.3 | 8 | Standard-M = the standard model of integers | n/a | T |
| **Definition 3.4** | 9 | **Generic Configuration** — the 5-tuple `(L^ξ, Δ^ξ_0, B^ξ, d, g)` | n/a | T |
| Example 3.5 | 9 | `Seq`/`STRING` machinery for the `Δ^A_0 → Δ^ξ_0` translation | n/a | T |
| Lemma 3.6 | 10 | Every `Δ^A_0` formula translates to an equivalent `Δ^ξ_0` formula in Standard-M, for every ξ | **cited** to Paris–Dimitracopoulos [28]; "the remainder of this article will never use them again" | T |
| Example 3.7 | 11 | Generic-configuration illustration | n/a | T |
| Definition 4.1 | 12 | `E(n)` denotes `2^n` | n/a | T |
| Definition 4.2 | 13 | `Scope_E(Υ,N)` — rebound unbounded quantifiers by `E(N)` | n/a | T |
| Definition 4.3 | 13 | **Good(N)** | n/a | T |
| Definition 4.4 | 13 | **♯(Υ)**, ♯(θ) | n/a | T |
| Definition 4.5 | 14 | **Tight**: `Log(q_β) ≥ ♯(β) + 2` | n/a | **V** |
| Lemma 4.6 | 14 | ξ Tight ⇒ `B^ξ + SelfRef(B^ξ,d)` consistent | **informal** — "kept brief and informal" | T |
| **Definition 5.1** | 16 | **A-Stable** (∗), constant ½ | n/a | **V** |
| Remark 5.2 | 16 | Short proofs give "partially useful" deductions even from untrue θ | n/a | **V** |
| **Definition 5.3** | 16 | **E-Stable** (∗∗) | n/a | **V** |
| Remark 5.4 | 16 | The `Π`/`Σ` polarity asymmetry between ∗ and ∗∗ | n/a | **V** |
| Definition 5.5 | 17 | **EA-stable** | n/a | T |
| Definition 5.6 | 17 | **Level(k^ξ) Consistent** | n/a | T |
| Definition 5.7 | 17 | **`SelfCons_k(β,d)`** | n/a | T |
| Remark 5.8 | 17 | `SelfCons_k` too strong for `k ≥ 2`; stronger than `SelfRef` at `k = 1` | n/a | T |
| **Theorem 5.9** | 18 | ξ EA-stable ⇒ `B^ξ + SelfCons_1(B^ξ,d)` self-justifying | **full** (≈1 page) | **V** |
| Definition 5.10 | 19 | **0-Stable** (∗∗∗) | n/a | T |
| Theorem 5.11 | 19 | ξ 0-stable ⇒ `B^ξ + SelfCons_0` self-justifying | **deferred to Appendix C** — "proof is similar to Theorem 5.9's" | T |
| Remark 5.12 | 20 | 5.9 covers three of Appendix D's examples, 5.11 the fourth | n/a | T |
| Lemma 6.1 | 20 | Adding true `Π^ξ_1` sentences preserves each stability property (i)–(iv) | **partial** — Appendix B proves only claim (i) in detail | T |
| Definition 6.2 | 20 | `G^ξ_k(θ)`; **Consistency Preserving** | n/a | T |
| Theorem 6.3 | 21 | `G^ξ_1` preserving when EA-stable; `G^ξ_0` when A-/E-/0-stable | **full** (via Lemma 6.1 + Thms. 5.9/5.11) | T |
| Remark 6.4 | 21 | Most ξ fail the hypothesis; footnote 11 ties failure to the Hilbert–Bernays conditions | n/a | T |
| Definition 6.5 | 22 | **Group-2 Schema** for `(B,D)` | n/a | T |
| Theorem 6.6 | 22 | Self-justifying system proving all of `(B,D)`'s `Π^ξ_1` theorems | **full** | T |
| Example 6.7 | 23 | `Test^ξ_0 =def ¬NegPrf^ξ` | n/a | T |
| Definition 6.8 | 23 | `TestList^ξ`, `GlobSim^D_B(ξ,j)` | n/a | T |
| Remark 6.9 | 24 | Group-2 versus GlobSim: neither dominates | n/a | T |
| Theorem 6.10 | 24 | Finitized `β_j` containing `GlobSim` | **full** | T |
| Remark 6.11 | 25 | **The open question**: can a finite extension prove all `Π^ξ_1` theorems *purely*? → Appendix G | n/a | T |
| **Theorem 6.12** | 26 | **Translational Reflection Principle** | **full** (short) | T |
| Remark 6.13 | 26 | Gödel's centennial caveat; (34) as a partial resolution | n/a | T |
| Remark 6.14 | 27 | How these logics grapple with the Gödel sentence `✵` | n/a | T |
| Remark 6.15 | 27 | Alternatives: Beklemishev, Kreisel–Takeuti CFA, Verbrugge–Visser | n/a | T |
| Remark 6.16 | 28 | **The two barriers** — (a) Theorem 2.1 and its tableaux analogues; (b) `SelfCons` yields "essentially a 1-line proof", i.e. "instinctive faith" rather than a full proof-justification | n/a | T |
| Facts B.1–B.3 | 33 | Interim facts for Lemma 6.1 | full | T |
| Lemmas C.1, C.2 | 35–36 | Modifications carrying Theorem 5.9's proof to Theorem 5.11 | full | T |
| Definition D.1 | 38 | The configuration ξ\*, incl. Eq. (46) and the **Conventional Tableaux Encoding Requirement** | n/a | **V** |
| Definition D.2, Fact D.3 | 41–43 | `Normed{a,b}`-style machinery imported from [64] | full | T |
| Theorem D.4 | 44 | **ξ\* is EA-stable** | **full** (the per-configuration work, ≈4 pp.) | T |
| Remark D.5 | 45 | Multiplication-totality as the trigger-point | n/a | T |
| Theorem E.1, Remark E.2 | 50–51 | The translational reflection principle is **inoperative** for conventional logics satisfying G2 | full | T |
| Definition G.1 | 55 | **`Braced^ξ(Φ,j)`** | n/a | T |
| Theorem G.2 | 56 | Three added sentences give pure knowledge of the Braced subset + self-consistency | **full** | T |
| Theorem G.3 | 57 | For any `c`, all `Size^ξ(c)` theorems provable purely, for each of the four ξ | **sketch** ("Proof Sketch") | T |
| Remark G.4 | 58 | Closing commentary | n/a | T |

## 5. Notation table

| Symbol | Meaning | Anchor |
| --- | --- | --- |
| ξ, `(L^ξ, Δ^ξ_0, B^ξ, d, g)` | generic configuration | Def. 3.4 |
| `Δ^ξ_0`, `Π^ξ_n`, `Σ^ξ_n` | the configuration-relative formula classes | Def. 3.1 |
| `Δ^A_0` / `Δ^R_0` | textbook `Δ_0` / bounded quantifiers using only `Maximum` | Ex. 3.2 |
| Standard-M | the standard model of integers | Def. 3.3 |
| `E(n)` | a term denoting `2^n` | Def. 4.1 |
| `Scope_E(Υ,N)` | unbounded quantifiers rebounded by `E(N)` | Def. 4.2 |
| Good(N), `♯(Υ)` | envelope truth; the largest such N | Defs. 4.3–4.4 |
| Tight | `Log(q_β) ≥ ♯(β)+2` | Def. 4.5 |
| R-View θ, RE-Class(ξ) | an r.e. set of `Π^ξ_1` sentences (not necessarily true); all of them | §5 |
| A-/E-/EA-/0-Stable | the four stability properties | Defs. 5.1, 5.3, 5.5, 5.10 |
| `Level(k^ξ)` | consistency level | Def. 5.6 |
| `SelfRef(α,d)`, `SelfCons_k(β,d)` | the two "I am consistent" axioms | §1, Def. 5.7 |
| `G^ξ_k(θ)` | `θ ∪ B^ξ ∪ SelfCons_k{[θ ∪ B^ξ],d}` | Def. 6.2 |
| `Check^ξ`, `Test^ξ_j`, `NegPrf^ξ`, `GlobSim^D_B(ξ,j)` | global-simulation machinery | §6, Def. 6.8 |
| `Reflect_{α,d}(Ψ)`, `Reflect^T_{α,d}(Ψ)`, `Ψ^T` | the two reflection principles and the translation | Eqs. 33–34 |
| `Neg^k`, `Prf^d_β`, `ExPrf^d_β`, `Subst`, `SubstPrf^d_β`, `Γ^k(g)`, `n̄` | fixed-point machinery | App. A |
| `Braced^ξ(Φ,j)`, `Count(Ψ)`, `Size^ξ(c)` | Appendix G machinery | Defs. G.1, §G |
| ξ\*, ξ\*\*, ξ⁻, ξ^R | the four sample configurations | App. D |
| Type-M / Almost-M / A / S / NS | the growth taxonomy | §1 |

## 6. Replicated context

| Block | Relation |
| --- | --- |
| `SelfRef`/Kleene–Rogers–Jeroslow framing | Recurs from 1993 §1, 2005 §1 |
| Definable cuts, cut-localized consistency (Eqs. 6–7) | Recurs from 2005 §4's survey, abbreviated |
| Theorem 2.1 (Solovay) | Third distinct formulation in the corpus — see drift D19 |
| U-Grounding functions, `Mult` graph via division (Eq. 45) | **Verbatim from 2005 §2** (Eq. 45 = 2005 Eq. 4) |
| The eight tableau rules; `Prenex`/`Reverse` | **Verbatim from 2005 §2** |
| Group-2 schema | Explicitly "consistent with [59, 61, 64, 66]'s notation" |
| `SubstPrf` fixed point (Eqs. 35–37) | 2005 Eqs. (7)–(8) with `k` added; 1993-TR (A.1)–(A.3) with `g` parameterized |
| `Tab−U*₁` | **Definition not repeated** — deferred to [64] |
| `Normed{a,b}`, Fact D.3 | Imported from [64] for the ξ\* stability proof |

## 7. Discrepancies and errata

- **D18 — "Self Justifying" changes referent.** 2011 §1 makes it a property of
  the *pair* `(α,d)`; `Willard2005` p. 2 makes the *pair* "Introspectively
  Unified" and reserves "Self-Justifying" for α alone (∃d). Compounds D7.
- **D19 — a third formulation of Solovay's theorem.** 1993-TR (Gentzen
  sequent calculus with cuts; Subtraction, non-zero Division, Successor total);
  2005 Theorem ∗ (Successor-Based Arithmetic; Hilbert); 2011 Theorem 2.1
  (Type-S axiom with `x'≠0`, `x'=y' ⇔ x=y`; `+`,`∗` as 3-way relations with
  the usual algebraic properties). All attributed to the same unpublished
  April-1994 private communication.
- **D20 — `Willard2001`'s title is mis-cited in the corpus.** The paper's own
  title page reads "Self-Verifying **Axiom** Systems, the Incompleteness
  Theorem and **Related Reflection Principles**". `Willard2005` ref. [67]
  calls it "…and **the tangibility reflection principle**"; 2011 ref. [61]
  gets the subtitle right but drops "Axiom". Our `paperlist` inherited the
  2005 form. Corrected in this component.
- **Version note.** The witness is **v8**; arXiv shows eight revisions between
  Aug and Dec 2011. No journal version is known (DBLP lists none). The
  codified statement should cite it as an arXiv preprint, not a paper of
  record.
- Typographical: "underlied behind" (p. 3), "irregardless" (used throughout),
  "principle"/"princible" (p. 26), "Zermello Fraenkel" (1993-TR usage recurs).

## 8. Saturation record

| Pass | Date | Method | Items | Result |
| --- | --- | --- | --- | --- |
| 1 | 2026-08-21 | Full text-layer read of all 64 pp.; permissive label sweep across both `N.M` and `A.N` numbering; bibliography resolution for [59]–[68] | 47 numbered items | Inventory complete |
| 1v | 2026-08-21 | **Visual control** of printed pp. 14, 16, 18, 32, 39 (Tight; A-/E-Stable; Theorem 5.9; Appendix A Eqs. 35–37; the coding condition) | 0 new | All confirmed; pagination offset established |
| 2 | *pending* | Saturation re-pass; visual verification of Appendix D's Theorem D.4 proof and Appendix E | — | — |


## Verification pass, 2026-08-27

Coverage recorded (pp. 1–64; the range is this record's own pass-1 "full
text-layer read of all 64 pp.", corroborated by an uncapped inventory sweep and
a re-read of §§1, 4 and Appendices A–C). Three corrections:

1. **Nine items had no `results.md` row** — Facts B.1–B.3, Lemmas C.1–C.2,
   Definition D.2, Fact D.3, Remark E.2, Remark G.4 — because §4's inventory
   carries them as *combined* rows. Now added individually.
2. **Lemma C.1's statement**, previously paraphrased loosely: every generic
   configuration that is E-stable or A-stable **automatically satisfies
   0-stability**, since `Scope_E(Υ,N) ≡ Υ` for `Δ^ξ₀` `Υ`.
3. **Type-Almost-M is the theorem-versus-axiom distinction** (p. 3), not a
   totality-strength one: α proves `∀x∀y∃z x+y=z` and `∀x∀y∃z x*y=z` **as
   theorems while treating none of (1)–(5) as axioms**, with those two written
   using genuine `+` and `*` function symbols. **That is the same axis
   `Willard2020`'s result turns on** (drift D29, sharpened).

Also recorded: Appendix A's "Reminder about Equation (37)" explains the `k ≤ 1`
restriction — `SelfCons_k` is **false** under Standard-M for nearly all `(β,d)`
once `k ≥ 2` — which is 2011's version of the ceiling `Willard2004` calls
Level(2+) and `Willard2005` calls `K = 2`.

## M1 exclusions

| Label | Reason |
| --- | --- |
| Claim * | Starred claim citation / display label, not a free-standing numbered item beyond existing Claim rows |
| Theorem 7.2 | Citation of an earlier paper's Theorem 7.2, not a 2011 numbered item |
