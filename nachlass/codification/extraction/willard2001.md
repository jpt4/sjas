# Extraction: Willard2001

> Spine extraction (component C7). Template: charter ADR-0001 §Decision.
> **Anchoring convention**: this witness is the **author's own repaginated
> copy**, numbered 1–67 and self-consistent (PDF page *n* = printed page *n*,
> verified at 9, 10, 22, 28, 51, 54, 60). Its pagination is **not** the JSL
> pagination (JSL 66(2), June 2001, pp. 536–596); no fixed offset relates the
> two, because Willard's cover note states he "used a larger type faunt".
> All anchors below are **this witness's** printed pages. Cite JSL section and
> item labels, never JSL page numbers, from this witness.
>
> **Text-layer hazards realised here** (charter §H2): `pdftotext` rendered the
> fraction `1/3` as `31` (p. 19), dropped every **overbar** on the Group-Zero
> constant symbols (p. 10), and rendered the valuation symbol **ϖ** as `$`.
> Every constant, bound and glyph below was read from a page image.

## 1. Identity and witness

| Field | Value |
| --- | --- |
| Corpus key | `Willard2001` |
| Title | Self-Verifying Axiom Systems, the Incompleteness Theorem and Related Reflection Principles |
| Venue | Journal of Symbolic Logic 66(2), 2001, pp. 536–596 |
| Witness | `nachlass/papers/willard2001_self_verifying_axiom_systems_author_jsl1.pdf` (67 pp., author's repaginated copy) |
| Text aid | `../sources-text/willard2001.txt` |
| Structure | §1 Introduction (2–8); §2 The Definitions of IS(A), ISREF(A) and IS^λ(A) (9–12); §3 Analysis of ISREF(A) (12–15); §4 The IS(A) Axiom System and the Intuition Behind IS^λ(A) (15–28); §5 The Consistency of IS^λ(A) (28–34); §6 The Tangibility Reflection Principle and Its Limitations (34–36); §7 Three New Variations of the Second Incompleteness Theorem (37–41); §8 Conclusion (41–43); Acknowledgments (43); Appendix A Solovay's Extension of Pudlák's Theorem (44–48); Appendix B The Turing-Function Encoding of Group-3 Axioms (48–55); Appendix C The Π⁻₁ Encoding for the Group-3 Axioms (55–61); Appendix D Proof of Theorem C.2 (61–63); Table I (63–64); Table II (65); References (65–67) |

### 1a. The witness is not textually identical to the JSL article

Willard's cover note claims the text is identical to the JSL 2001 article
except for the type size. **It is not**, at least in the bibliography:

- **[45]** cites *A Generalization of the Second Incompleteness Theorem and
  Some Exceptions to It*, **APAL 141 (2006)** — five years after publication.
  It is cited **nowhere in the body**; it appears only in the reference list.
- **[44]** still carries the pre-publication note that a longer version of the
  TABLEAUX-2000 paper "will appear soon in the JSL" (that became
  `Willard2002a`), so the update was **partial**.

Consequence for the codification: this witness is authoritative for the
mathematics and for Willard's own later view of his citations, but **not** for
"what the JSL 2001 text said". Logged as gap **G15**; drift **D22**.

## 2. Role in corpus

2001 is the **first full journal statement** of the SJAS program, expanding the
1993 KGC extended abstract [40] and TR 93-10 [41] into full proofs, and adding
the reflection-principle line that TR 93-10 only gestured at. Its role has four
parts:

1. **It defines the tangibility reflection principle**, the construct that
   `Willard2005` and `Willard2011` presuppose and never redefine.
2. **It supplies the Δ⁻₀ arithmetization** (Appendices B–D and Table I) that
   both later papers delegate to rather than repeat.
3. **It contains Theorem 7.2**, the Löb-variant `Willard2011` cites.
4. **It is the corpus's only systematic positioning statement** against the
   definable-cut tradition (§1's criteria I–VII, §3.9 below).

Willard's own retrospective framing (p. 43): the four generalizations of the
Second Incompleteness Theorem (A)–(D) and his Theorems 7.2–7.4 together
"show that it is **impossible for a self-verifying system to be optimal from
all of perspectives (I)-(VII) simultaneously**".

## 3. Systems and machinery defined

### 3.1 Vocabulary

**"IS" abbreviates "Introspective Semantics"** (p. 9) — the only place in the
corpus where the acronym is expanded.

`F(a₁,…,a_j)` is a **NON-GROWTH FUNCTION** iff `F(a₁,…,a_j) ≤ Maximum(a₁,…,a_j)`
for all arguments (p. 9). The **seven GROUNDING FUNCTIONS** (p. 9, image-verified):

| # | Function | Definition |
| --- | --- | --- |
| 1 | Integer Subtraction | `x − y = 0` when `x < y` |
| 2 | Integer Division | `x/y = x` when `y = 0`, else `⌊x/y⌋` |
| 3 | `Predecessor(x)` | `Max(x − 1, 0)` |
| 4 | `Maximum(x, y)` | — |
| 5 | `Logarithm(x)` | **`⌈Log₂(x + 1)⌉`** |
| 6 | `Root(x, y)` | **`⌊x^{1/y}⌋`** when `y ≥ 1`; `Root(x,0) = x` |
| 7 | `Count(x, j)` | number of "1" bits among `x`'s rightmost `j` bits |

**The count seven is not load-bearing.** Willard states on p. 10 that Group-1
may be expanded by "any larger set `F` of additional non-growth functions that
is axiomatized by any finite set of Π⁻₁ axioms (and the Group-2 scheme is
accordingly adjusted)", and all theorems still hold. This settles the
seven-versus-six question against `Willard2005`/`Willard2011` as **presentational,
not substantive** — see drift D23.

Class notation: `Δ⁻₀` (all quantifiers bounded), `Π⁻₁`, `Σ⁻₁` — the analogues of
`Δ₀/Π₁/Σ₁` with Addition and Multiplication replaced by the grounding functions
(p. 9). Appendix B adds **`Δ⁺₀`, `Π⁺₁`** for formulas also using the Turing
functions (p. 50). A sentence is **PRENEX\*** iff its quantifier block admits
bounded as well as unbounded quantifiers (Definition 1.1).

### 3.2 The four axiom groups

**Group-Zero** (p. 10, overbars image-verified — `n̄` is the constant symbol for
`n`): one constant symbol `n̄` per natural `n ≥ 0`; axioms `1̄ ≠ 0̄` and, for each
`n > 0`,

```
Predecessor(n̄) = ‾(n−1)      ‾(2n) − n̄ = n̄      ‾(2n+1) − n̄ − 1̄ = n̄
```

(`2n` is *twice* `n`, not `2ⁿ`.)

**Group-1**: the **Π⁻₂** totality axiom for Addition (Eq. 6)

```
∀x ∀y ∃z   z − x = y
```

— restated in §4 as Eq. (17) `∀x ∀y ∃z  x = z − y`, and in §5 as Eq. (32); the
three are the same axiom in three presentations. The remaining Group-1 axioms
are Π⁻₁ sentences fixing `=` and `<` and evaluating every grounding function on
constants. **Table I** (pp. 63–64) exhibits one such finite set — **30 axioms** —
and this is exactly what `Willard2005`'s Group-1 delegates to.

**Group-2** (Eq. 7): with `T` a translation from IS(A)'s language to `A`'s and
`TransProof_A(x,y)` a Δ₀ formula,

```
∀y { TransProof_A(⌜Φ⌝, y) ⊃ Φ }      for each Π⁻₁ sentence Φ
```

Footnote 1 gives the reason for the generic form: it keeps the axiom-recognition
predicate `Ax(s)` **Δ⁻₀**, which simply listing `A`'s theorems would not.
Footnote 2 connects `T` to definable cuts (`T_J : Φ ↦ Φ^J`), noting PA and ZF
admit only one definable cut while GB and IΣ₀ admit infinitely many.

**Group-3**: system-specific; see §3.3.

### 3.3 The systems

2001 defines **nine** systems — far more than 2005 or 2011 — of which three are
principal:

| System | Group-1 totality | Apparatus | Group-3 | Status |
| --- | --- | --- | --- | --- |
| `IS(A)` | Addition | Semantic Tableaux | Eq. (8) `∀y ¬SemPrf_{IS(A)}(⌜0=1⌝, y)` | consistency-preserving, Thm 4.3 |
| `IS^λ(A)`, `0.01 < λ < 1` | Addition | Semantic Tableaux | Eq. (9) `∀x∀y∀z {SemPrf(⌜Ψ⌝,y) ∧ y^λ < z/x ⊃ Ψ^x_z}` | consistency-preserving **at λ = 3/4 only**, Thm 5.1 |
| `ISREF(A)` | **none** (Addition dropped) | **Hilbert** | Eq. (10) `∀x∀y {HilbPrf(⌜Ψ⌝,y) ∧ Size(y) ≤ x−1 ⊃ Ψ^{x−1}_{x−1}}` | consistency-preserving, Thm 3.4 |
| `ISTR(A)` | **Bitwise-Or** | Hilbert | Eq. (16) `… Size(y) < x ≤ z/2 ⊃ Ψ^x_z` | proofs deferred to [43] (Remark 3.8) |
| `ISM(A)` | **Multiplication** | Semantic Tableaux | Eq. (61) `… y·Log₂(x+2) < Log₂(z) ⊃ Ψ^x_z` | consistency-preserving "by a routine generalization"; **cannot** prove +++ (§6) |
| `ISMULT(A)` | Multiplication | Semantic Tableaux | "no tableaux proof of 0=1 from ISMULT(A)" | engine **breaks** (Remark 4.5); later shown outright **inconsistent** for every `α ⊇ PAX` in [44] |
| `XIS^λ(PAX)` | Multiplication | Semantic Tableaux | Eq. (71), self-referential | **inconsistent for all λ** (Thm 7.3) |
| `ISREF^U(A)`, `IS^{λU}(A)` | as parents | as parents | uniformised Group-3 | support the **Uniform** Tangibility Reflection Principle Eq. (60); consistency "quite similar", **not proved here** (Remark 6.3) |

`Size(y)` = the largest constant stored in proof `y`. `UNION(A)` = the union of
Groups Zero, 1 and 2 (Appendix B).

**Admission condition.** `A` is **Regularly Consistent** iff `Prf_A(x,y)` has a
Δ⁻₀ encoding **and** the generic Group-2 axioms (7) are **valid** (true in the
standard model) for every Π⁻₁ sentence `Φ` (p. 12). A mapping `I(•)` is
**Consistency-Preserving** iff `I(A)` is consistent whenever `A` is regularly
consistent. This is 2001's counterpart of 2005/2011's "Π\*₁-true" hypothesis —
see drift D24.

### 3.4 Tangibility, and the apparatus ↔ predicate ↔ totality trade-off

**Definition 1.2** gives three tangibility predicates (`k ≥ 2`):

```
TangPred(x)    ≡  ∃v  x < v − 1
TangDiv_k(x)   ≡  ∃v  x < v/k
TangRoot_k(x)  ≡  ∃v  x < v^{1/k}
```

and the **Tangibility Reflection Principle** for `Ψ` (Eq. 2):

```
∀x { [ ∃y Prf_α(⌜Ψ⌝, y) ∧ Tangible(x) ] ⊃ Ψ^x }
```

The three predicates are **not stylistic alternatives**. §1's results (i) and
(ii) (pp. 4–5) index them to apparatus strength and totality budget:

| Apparatus | Tangibility predicate | Totality afforded | System |
| --- | --- | --- | --- |
| Hilbert / Gentzen **with cuts** | `TangPred` | neither Addition nor Multiplication | `ISREF(A)` |
| Hilbert / Gentzen with cuts | `TangDiv₂` | **Bitwise-Or** | `ISTR(A)` |
| **cut-free** (Herbrand, Tableaux) | `TangRoot` | **Addition** | `IS^λ(A)` |

This table is the single most compressed statement of the corpus's design
space and has no counterpart in 2005 or 2011. Obligation **O28**.

Why tangibility is enough (p. 8): for a **Σ⁻₁** sentence `Ψ` we have `Ψ ≡ Ψ^0`,
provably in essentially any α, so Eq. (2) yields Eq. (4) `{∃y Prf_α(⌜Ψ⌝,y)} ⊃ Ψ`
and, with `Ψ = "0=1"`, the flat statement Eq. (5) `∀y ¬Prf_α(⌜0=1⌝, y)`.

### 3.5 Definable Cut versus Tangibility Criteria

`Υ(v)` is a **Definable Cut** for α iff α proves (a) `Υ(k̄)` for each fixed `k`,
(b) `∀u∀v Υ(v) ∧ u < v ⊃ Υ(u)`, and (c) `∀u Υ(u) ⊃ Υ(u+1)`. If α proves only
(a) and (b), `Υ` satisfies α's weaker **Tangibility Criteria** (pp. 5–6).

**Tangibility is a cut minus closure under successor.** This one clause is the
whole difference between Willard's program and the Nelson / Pudlák /
Wilkie–Paris tradition, and it is why `ISREF(A)` — which does not recognise
Successor as total — cannot view `TangPred` as a definable cut (criterion III),
while `IS^λ(A)` *can* view `TangRoot` as one. Obligation **O29**.

### 3.6 The engine: (k,m)-Affirmative Branches

Notation (pp. 17–18): `VALUE(c_K) = K` if `c_K` is *locally defined* in `y`
(appears in some proper axiom of the proof tree), else `0`; `VALUE(u)` free for
parameters; `CONS(y)` = largest `VALUE(c_K)` over constant symbols in `y`
(parameters excluded). `Ψ̃` replaces each parameter `u` by the constant
`VALUE(u)`; `Ψ̃⇓m` further bounds `Ψ̃`'s unbounded quantifiers by `m`.

**Lemma 4.1**: `VALUE(c_K) < √y` — a constant too large cannot fit in the proof
that mentions it.
**Lemma 4.2**: on an *Addition-Conservative* branch, `VALUE(u) < y`, because the
fastest growth is doubling: `VALUE(u_i) = 2^i·CONS(y)`, with `n < (1/3)Log₂ y`
and `CONS(y) < √y`, giving `y^{1/3}·y^{1/2} = y^{5/6} < y`.

A branch `σ` is **(k,m)-AFFIRMATIVE** (p. 22, image-verified) iff for some
valuation ϖ:

- **I.** every sentence `Ψ` on `σ` has `Ψ̃⇓m` valid, *unless* `Ψ` is a **Special
  Exception** — the addition-totality axiom (17) and its Primary (18) and
  Secondary (19) deductions, but **not** its Tertiary deduction (20);
- **II.** with `β_i` = the bit-length Appendix B assigns to the first `i` depth
  levels of `σ`, every parameter `u` at node `N_i` satisfies

```
VALUE(u)  <  ( k + √y ) · 2^{⌊β_i/6⌋ − 2}                     (26)
```

**Lemma 4.6**: a candidate tree containing a (k,m)-Affirmative branch is not a
tableaux proof — because a closed branch carries `Ψ` and `¬Ψ`, footnote 8 forces
such a `Ψ` to be **Δ⁻₀**, and `Ψ̃⇓m` and `¬̃Ψ⇓m` cannot both hold.
**Lemma 4.7**: the bound extends from parameters to all terms (uses "our seven
Grounding functions are non-growth").
**Lemma 4.8**: if `y` is the *least* tableaux proof of `0=1` from `IS(A)`, then
with `k = 0`, `m = y − 1`, `y` contains a (k,m)-Affirmative branch — proved by
an **eleven-case** induction on the tableaux rules.
**Theorem 4.3** then follows in five sentences from 4.6 + 4.8.

**Case 3 is the joint of the whole engine.** The Group-3 axiom `Γ` is *false*,
but `Γ̃⇓m` is *true* because `m + 1 = y` is the least proof. Footnote 9 states
the condition that makes this non-vacuous: the other ten cases must guarantee
`VALUE(t) ≤ m` for every term `t` — which they do via `k = 0`, `m = y − 1`,
`β_j ≤ Log₂ y` and Lemma 4.7.

**Why the density constant is load-bearing** (Case 11, p. 28, image-verified).
The Tertiary deduction (34) `t₁ = u − t₂` sets `VALUE(u) = VALUE(t₁) + VALUE(t₂)`,
at most **doubling** the bound, i.e. costing one unit of `2^{β/6}`. Willard then
observes that encoding sentence (34) "requires substantially more than six bits",
so `β_i − β_{i−1} ≥ 6`, and the factor 2 is absorbed exactly:

```
VALUE(u) < 2·(k+√y)·2^{⌊β_{i−1}/6⌋−2}   ⟹   VALUE(u) < (k+√y)·2^{⌊β_i/6⌋−2}
```

**The budget covers addition and nothing faster.** This is the clearest
statement in the corpus of why coding density is a hypothesis and not a
convenience, and it is the origin of drift D6. Obligation **O30**.

**Remark 4.5** localises the multiplication failure exactly: `ISMULT(A)` still
satisfies Assertion ++, but has **no analogue of Lemma 4.2**, since footnote 7's
`u₀ = 2`, `u_{i+1} = u_i²` gives `VALUE(u_n) = 2^{2^n}`.

**Remark 4.4 / Assertion ++.** The intuition is a "**Vacuous Truth**": any proof
`y` of `0=1` yields a pair `(p,u)` with `p ≤ VALUE(u) < y` and `p` another proof
of `0=1`, contradicting minimality. Willard states that **TR 93-10 [41] and the
1993 abstract [40] "contained intermediate results that were essentially
equivalent to the Assertion ++"**, and that Lemma 4.8 is *strictly stronger*
than ++. This confirms the C5 reading of the TR's Lemma 6.2 as the ancestor.

**§5 reuses the machinery portably**: Lemma 5.3 runs the same eleven cases with
`k = x`, `m = z − 1` (plus one new sub-case, since Lemma 5.3's tree root may be
an arbitrary prenex\* sentence rather than `0≠1`). This is why (k,m) is carried
as a parameter (stated at p. 24).

### 3.7 The reflection results (§6) and the negative side (§7)

**Theorem 6.1**: if `0 < λ < 1`, `k > 1/(1−λ)` and `A` proves the Π⁻₁ sentence
(55) `∀x∀y∀v {x < v^{1/k} ⊃ y^λ < MAX(v,y)/x}`, then `IS^λ(A)` has a **Hilbert**
proof of its `TangRoot_k` reflection principle. The derivation is four lines:
substitute `MAX(v,y)` for `z` in Group-3, apply (55), then **Lemma 3.1.C**
(`Φ^i_j ⊃ Φ^i`). **Corollary 6.2** transfers this to semantic tableaux by
Gentzen cut-elimination.

**The multiplication dichotomy (§6, pp. 36).** `ISM(A)` *does* recognise
Multiplication as total and *is* consistency-preserving — but its wider gap
`y·Log₂(x+2) < Log₂ z` is "too weak" to prove

```
+++   "I am unable to produce a Semantic Tableaux proof of 0=1"
```

Footnote 14 gives exactly what survives: `∀y [∃z y < Log₂ z] ⊃ ¬SemPrf_{ISM(A)}(⌜0=1⌝,y)`.
Willard's conclusion: "ISM(A) and IS^λ(A) have quite different philosophical and
epistemological implications, although the strictly formalistic mathematical
proofs of their consistency are virtually identical."

**Consistency-preservation and self-knowledge are two properties, and
multiplication separates them.** Obligation **O31**.

**Lemma 7.1** replaces the Hilbert–Bernays derivability conditions with a
*semantic* closure property available to any α-sound and α-complete `D`: from
`α ⊢ φ`, `α ⊢ ψ` and `φ ∧ ψ ⊃ ϕ` true in every model of α, infer `α ⊢ ϕ`. This
is why §7's theorems need **no arithmetic inside α at all**.

**Theorem 7.2**: α consistent, `D` α-sound and α-complete, `Prf^D_α` a Δ⁻₀
formula, α proves all PAX's Π⁻₁ theorems ⟹ α cannot prove the **Canonical**
Reflection Principle (62) for every Π⁻₁ sentence. Fixed point:
`Γ(g) = ∀x ∀h ≤ x  SUBST(g,h) ⊃ ¬Prf^D_α(h,x)`, `Θ = Γ(n̄)` — the bounded `∀h ≤ x`
is what keeps `Θ` **Π⁻₁**. Willard's stated difference from Löb: "we do not
assume that α recognizes either Addition or Multiplication as total functions."

**Theorem 7.3**: `XIS^λ(PAX)` (= `IS^λ(PAX)` + Multiplication total, Group-3
self-referential) is **inconsistent for every λ** — multiplication makes
`∀x∀y∃z y^λ < z/x` provable, collapsing the Group-3 guard into full canonical
reflection (75), which Theorem 7.2 forbids.

**The inconsistency is syntactic and the pair is nameable.** Theorem 7.3
establishes (75) *unconditionally* — no consistency assumption enters. Theorem
7.2's proof, instantiated at α = `XIS^λ(PAX)` and D = semantic tableaux, then
derives **both** (68) `α ⊢ Θ` and (70) `α ⊢ ¬Θ` for `Θ = Γ(n̄)`. Three
qualifications belong with that:

- **The witness step is metatheoretic.** (68) → (69) leaves α: because α proves
  `Θ`, a proof object exists, so `SemPrf_α(⌜Θ⌝, m̄)` is a *true Δ⁻₀ sentence*
  and α proves it. What is established is that two derivations **exist**; no
  size bound is given for either, and no single contradiction-derivation is
  exhibited. This is the exact converse of the positive side of the corpus,
  where every argument is a minimal-witness construction carrying explicit
  bounds. The asymmetry should be stated, not smoothed over — obligation O37.
- **Inconsistency is of the axiom set, hence apparatus-independent.** Willard
  writes `⊢_D` in Theorem 7.2 and bare `⊢` in Theorem 7.3; it does not matter,
  since Lemma 7.1 requires `D` α-sound and α-complete, and for such `D` proving
  `Θ` and `¬Θ` means the axioms have no model. Unusual in this corpus, where
  apparatus choice is normally decisive.
- **No step covertly assumes consistency.** Lemma 7.1B argues semantically
  (soundness, then completeness) and stays valid vacuously when α has no
  models, so the refutation is not circular.

**Observation not made by Willard**: (71)'s consequent is **`Θ^x`**, where
Eq. (9)'s is `Ψ^x_z`. By Lemma 3.1.C, `Ψ^x_z ⊃ Ψ^x`, so (71) is a **weaker**
axiom than the literal analogue of `IS^λ`'s schema — and the system is
inconsistent anyway, which makes Theorem 7.3 stronger than a like-for-like
reading suggests. Willard does not remark on the difference; whether it is
streamlining for the proof or inadvertent is undetermined. Recorded as erratum
11 in §7.

**Theorem 7.4** (Proof Sketch): α consistent, Multiplication total, plus the
proof-compression hypothesis (76) ⟹ α cannot prove the **Second Uniform**
Reflection Principle (77) for all Δ⁻₀ `φ`. Footnote 15 warns (76) can fail for
unusual tableaux systems.

**Remark 3.6 states the margin**: Theorem 7.2 implies no consistent axiom system
can prove Theorem 3.5's Eq. (15) with the superscript `y − 1` replaced by `y`.
**`y−1` is attainable and `y` is not.** Obligation **O32**.

### 3.8 The arithmetization (Appendices B–D)

**The encoding** (p. 51, image-verified): **a byte is six bits**; 21 atomic
symbols each get a 6-bit code with **leading bit 1**; the constant for `i` is
`⌈log₃₂(i+1)⌉ + 1` bytes — a `Ĉ` tag byte then `i` in **base 32** with leading
bit 0 in each byte; a formula's integer encoding is a number in **base 64**.

**This reconciles what looks like a density drift across the corpus.** Six bits
per byte and five payload bits per byte are two measurements of *one* scheme:

| Paper | Constant | What it counts |
| --- | --- | --- |
| `Willard2001` | `2^{β/6}`, `β_i − β_{i−1} ≥ 6`, `Log₆₄ y` | **bits per byte** |
| `Willard1993`/`-TR` | six-bit bytes, base 32; `i* > i^{6/5}` | **the ratio** 6/5 |
| `Willard2005` | `U-Height(p) < (1/5)Log₂ p` (Eq. 20) | **payload bits per byte** |
| `Willard2011` | Gödel number `≥ 32^J` | **payload**, base 32 |

No paper contradicts another. Recorded as drift **D25** *resolved-on-sight*, and
as obligation **O33**: the codified statement must present one encoding with a
stated bits/payload distinction, or the four constants will read as four
incompatible requirements.

**The fixed point** (Appendix B). `Θ(g) = ∀y {¬SubstSemPrf_{UNION(A)}(⌜0=1⌝, y, g)}`;
with `N = ⌜Θ(g)⌝`, IS(A)'s Group-3 axiom **is** `Θ(N)`. For the *schematic*
Group-3 of `IS^λ(A)` and `ISREF(A)`, Willard introduces **pseudo-formulas**
carrying placeholders `♣`, `⌜♣⌝`, `♣^z_x`, with `PseudoTransform₁/₂` and
`ExSemPrf_α(s,y,g)` / `ExHilbPrf_α(s,y,g)`. This is the direct ancestor of
`Willard2011`'s `SubstPrf` / `Γ^k(n̄)` machinery.

**Theorem B.1** (Proof Sketch): `ExSemPrf_{UNION(A)}` is **Δ⁺₀**-encodable —
context-free recognition in `PolyLog(s+y+g)` ≪ `O(s+y+g)`.
**Theorem C.1** (Proof Sketch): **25** itemized predicates have **LinH**
decision procedures; items 13–20 are specified formally in **Table II** (p. 65).
**Theorem C.2**: every LinH decision procedure has a **Δ⁻₀** formula. Willard
calls this "implicit from the prior literature" (Wrathall [46] → rudimentary
formulas; Hájek–Pudlák [13] and Krajíček [18] → Δ₀ via Bennett [2]) and notes
his task is *easier*: because `Count` and `Logarithm` are **primitives** of Δ⁻₀,
he "simply avoid[s] the hardest part" — Bennett's exponentiation encoding and
the `NUON(x) = COUNT(x,∞)` construction. Appendix D proves it via **ks-vectors**
(`∃U` abbreviating `∃u₁≤s …∃u_k≤s`) simulating a linear-time multi-tape machine.
**The grounding-function set is chosen partly to make the arithmetization cheap.**
Obligation **O34**.

**Theorem C.3**: the Group-3 schemas of `IS^λ(A)`, `IS(A)`, `ISREF(A)` all have
**Π⁻₁** encodings. **Remark C.5** strengthens this: even `F`-extended (e.g.
Turing-function) versions still admit Π⁻₁ Group-3 axioms, so "the Turing
versions … have essentially no disadvantage".

### 3.9 The seven comparison criteria (§1, pp. 6–7)

Advantages of the definable-cut literature [20,21,24,28,36,38]:

- **I** Their systems (Q, ACA₀, IΣ₀, GB, Gentzen systems) are widely studied;
  Willard's are "especially manufactured with the assistance of Kleene's Fixed
  Point Principle".
- **II** They recognise Multiplication as total; `ISREF(A)` does not even
  recognise Successor.
- **III** Their `Tang(y)` are **definable cuts**; `ISREF(A)`'s `TangPred` is not.
- **IV** IΣ₀ proves that if SuperExp is total then IΣ₀ is consistent.

Advantages of 2001's systems:

- **V** Eq. (2) restricts only `x`, never `y`; so these α prove the **flat**
  `∀y ¬Prf_α(⌜0=1⌝, y)`, where Eq. (3)'s systems get consistency only *on a cut*.
- **VI** Reflection (2) is inherently broader than (3)'s non-existence claim.
- **VII** Result (i) permits `Prf` to be Hilbert **or** Gentzen-with-cuts.

> "there are inherent tradeoffs that prevent a self-justifying axiom system from
> possessing all of features (I) through (VII), simultaneously! Thus, it is
> futile to seek an idealized form of self-verifying system"

Obligation **O35** — this is a direct constraint on the Refinement stage's
ambition, stated by Willard about his own program.

The four negative results framing the space (pp. 3–4): **(A)** Pudlák — no
extension of Q verifies its own Hilbert consistency; **(B)** Solovay (private,
April 1994) — same for α merely recognising Successor as total; **(C)** Willard's
Theorem 7.2 — same for α recognising *none* of Addition/Multiplication/Successor;
**(D)** Adamowicz [1] (1999 circulating manuscript) — for each `m ≥ 2`,
`IΣ₁ + Ω_m` cannot prove its own semantic-tableaux consistency.

### 3.10 The motivating question

Stated twice (pp. 8 and 42):

> \* How do Human Beings manage to muster the physical energy and psychological
> desire to think (and prove theorems) when the … generalizations of Gödel's
> Incompleteness Theorem assert that no reasonable conventional axiom system can
> confidently assume its own consistency?

Willard's partial answer: "a Thinking Being can assume that if he proves Ψ, then
Ψ is valid when it is restricted to numbers of 'reasonable size'." **Remark 3.8**
sharpens it: `ISTR(A)`'s Bitwise-Or formalises finite-set **union**, and Group-2
supplies set-subtraction and intersection, so `ISTR(A)` recognises **the three
basic finite-set operations** as total alongside its Hilbert consistency and
PA's Π⁻₁ theorems — "Could this be a partial … explanation for how Human Beings
seem to have an instinctive sense of their Self-Consistency?" This is the
set-theoretic reading of SJAS and the earliest anchor for the 2008 ZCF/ZF
material. Obligation **O36**.

### 3.11 The conclusion's boundary catalogue (§8, p. 42)

Willard sketches **computable** variants whose "I" refers to a machine
generating ~`N` bits in `N` steps, and states without proof:

- feasible if it recognises **Successor** total and keeps Group-Zero's
  `Predecessor(n̄) = ‾(n−1)`;
- **inconsistent** if it recognises **Addition** total, or keeps Group-Zero's
  `‾(2n) − n̄ = n̄`;
- Successor-totality must be **removed** if Group-3 uses Hilbert rather than
  tableaux proofs.

These are the seed of the corpus's whole boundary-case line. All are
**stated-only** — gap **G16**.

## 4. Numbered-item inventory

Anchors are this witness's printed pages. Proof status vocabulary per charter:
`full` | `sketch` | `cited` | `stated-only` | `n/a`.

| Label | p. | Statement digest | Proof |
| --- | --- | --- | --- |
| Definition 1.1 | 4 | Prenex\* form; `Φ^i_j` bounds unbounded universals by `i`, existentials by `j`; `Φ^i ≡ Φ^i_∞` | n/a |
| Definition 1.2 | 4 | `Tangible(x)`; the three predicates `TangPred`, `TangDiv_k`, `TangRoot_k`; Tangibility Reflection Principle Eq. (2) | n/a |
| Lemma 3.1 | 13 | Four monotonicity laws: (A) `a≤b ∧ Φ^i_a ⊃ Φ^i_b`; (B) `a≥b ∧ Φ^a_j ⊃ Φ^b_j`; (C) `Φ^i_j ⊃ Φ^i`; (D) `a≥b ∧ Φ^a ⊃ Φ^b` | full |
| Lemma 3.2 | 13 | `Θ` a Π⁻₁ sentence with constants `≤ i`, valid in Standard Model ⟹ valid in the finite model `M_i` | sketch |
| Remark 3.3 | 13 | Converse for `Σ⁻₁`/`Δ⁻₀` sentences: valid in `M_i` ⟹ valid in Standard Model | stated-only |
| Theorem 3.4 | 13 | `ISREF(•)` is consistency-preserving | full |
| Theorem 3.5 | 14 | Given Eq. (13) `∀y Size(y) < y−1`, `ISREF(A)` proves its `TangPred` reflection principle for every prenex\* `Ψ` | full |
| Remark 3.6 | 15 | Theorem 7.2 forbids Eq. (15) with `y−1` strengthened to `y` — the margin is exactly one | cited |
| Remark 3.7 | 15 | Even Group-3 weakened to "I cannot produce a Hilbert proof of 0=1" is inconsistent if Successor is total (Pudlák/Solovay) | cited |
| Remark 3.8 | 15 | Defines `ISTR(A)` (Bitwise-Or total, Eq. 16); its analogues of Thms 3.4/3.5 are proved in **[43]**; finite-set reading | cited |
| Lemma 4.1 | 17 | `VALUE(c_K) < √y` for any tableaux proof `y` from `IS(A)` or `IS^λ(A)` | full |
| Lemma 4.2 | 19 | On an Addition-Conservative branch, every parameter satisfies `VALUE(u) < y` | sketch |
| Theorem 4.3 | 20 | `A` regularly consistent ⟹ `IS(A)` consistent; `IS(•)` is consistency-preserving | full |
| Remark 4.4 | 20 | The "Vacuous Truth" intuition and **Assertion ++**; credits [40]/[41] with equivalent intermediate results | n/a |
| Remark 4.5 | 21 | `ISMULT(A)`: satisfies ++ but has no analogue of Lemma 4.2; footnote 7's `2^{2^n}` counterexample | n/a |
| Lemma 4.6 | 22 | A candidate tree containing a (k,m)-Affirmative branch is not a tableaux proof | full |
| Lemma 4.7 | 23 | The Part-II bound extends from parameters to arbitrary terms | full |
| Lemma 4.8 | 23 | The least tableaux proof of `0=1` from `IS(A)` contains a `(0, y−1)`-Affirmative branch | full (11 cases) |
| Theorem 5.1 | 28 | For `λ = 3/4`: `A` regularly consistent ⟹ `IS^λ(A)` consistent | full |
| Lemma 5.2 | 29 | If Thm 5.1 fails at `λ=3/4` there is a tuple `(x,y,z,Φ)` satisfying Eq. (40) | full |
| Lemma 5.3 | 29 | The minimal-`z` such tuple makes `y` contain a `(x, z−1)`-Affirmative branch | full (11 cases) |
| Remark 5.4 | 34 | `λ < 1` is what §6 needs for the Tangibility Reflection Principle | n/a |
| Remark 5.5 | 34 | A tighter proof would give `λ = .01` "and probably any `λ ≥ 10⁻⁴` or even yet smaller" | stated-only |
| Remark 5.6 | 34 | Theorem 5.1 generalises to any cut-free method — Herbrand, Resolution, Cut-Free Sequent Calculus | stated-only |
| Theorem 6.1 | 34 | `0<λ<1`, `k > 1/(1−λ)`, `A` proves Eq. (55) ⟹ `IS^λ(A)` has a **Hilbert** proof of its `TangRoot_k` reflection principle | full |
| Corollary 6.2 | 35 | The same, with a **semantic tableaux** proof (via Gentzen cut-elimination) | full |
| Remark 6.3 | 35 | **Uniform** Tangibility Reflection Principle Eq. (60); systems `ISREF^U(A)`, `IS^{λU}(A)` support it; consistency proofs omitted | stated-only |
| Lemma 7.1 | 37 | For α-sound and α-complete `D`: (A) syntactic and (B) semantic closure under modus ponens | full |
| Theorem 7.2 | 38 | No consistent α with Δ⁻₀ `Prf^D_α` proving PAX's Π⁻₁ theorems can prove the Canonical Reflection Principle for every Π⁻₁ sentence | full |
| Theorem 7.3 | 40 | `XIS^λ(PAX)` is inconsistent **for all** `λ` | full |
| Theorem 7.4 | 41 | A Multiplication-total α satisfying compression hypothesis (76) cannot prove the Second Uniform Reflection Principle (77) for all Δ⁻₀ `φ` | sketch |
| Theorem A.1 | 44 | Hilbert–Bernays generalised: α proving PAX's Π⁻₁ theorems with `Der` satisfying the three derivability conditions cannot prove `¬Der(⌜0=1⌝)` | stated-only (footnote 16 argues identity with Hilbert–Bernays) |
| Theorem A.2 | 47 | **Solovay's Extension of Pudlák's Theorem**: a Successor-Regular α (B-adic proof encoding) cannot prove `∀p ¬HilbPrf_α(⌜0=1⌝, p)` | sketch |
| Theorem B.1 | 54 | `ExSemPrf_{UNION(A)}(s,y,g)` has a **Δ⁺₀** encoding | sketch |
| Remark B.2 | 54 | Hence `IS^λ(A)`'s Group-3 axioms are **Π⁺₁** | n/a |
| Remark B.3 | 54 | The same construction gives `ISREF(A)` a Π⁺₁ Group-3 via `ConSize(x,y)` | stated-only |
| Theorem C.1 | 56 | **25** itemized predicates have LinH decision procedures (item 25 is `ExSemPrf_{UNION(A)}`) | sketch |
| Theorem C.2 | 59 | Every LinH decision procedure is defined by a **Δ⁻₀** formula | sketch (Appendix D) |
| Theorem C.3 | 60 | The Group-3 schemas of `IS^λ(A)`, `IS(A)`, `ISREF(A)` have **Π⁻₁** encodings | full |
| Remark C.4 | 60 | `⌜(96)⌝ ≫ ⌜(95)⌝` is harmless: `N_λ` enters as a **fixed constant**, not a computed value | n/a |
| Remark C.5 | 61 | Even `F`-extended (Turing) versions keep **Π⁻₁** Group-3 axioms — "essentially no disadvantage" | stated-only |
| Table I | 63 | **30** Π⁻₁ axioms defining the grounding functions and the `=`/`<` predicates | n/a |
| Table II | 65 | LinH decision procedures for Theorem C.1's items 13–20 | n/a |

**Unnumbered but load-bearing**: Assertion ++ (p. 20); the negative results
(A)–(D) (pp. 3–4); criteria I–VII (pp. 6–7); Definable Cut / Tangibility
Criteria (p. 5); Regularly Consistent and Consistency-Preserving (p. 12);
Non-Growth Function and the seven grounding functions (p. 9); the eight semantic
tableaux deduction rules and the Φ-Based Candidate Tree (p. 16); Addition-
Conservative valuation and branch (p. 19); (k,m)-Affirmative branch (p. 22);
Successor-Regular (p. 45); the cut sequence `J₀`–`J₃` (Eqs. 79–82, p. 46).

## 5. Notation table

Feeds `../registry/notation.md`. See that registry for the canonical mapping.

## 6. Replicated context

- The Kleene-fixed-point framing of "why this is not trivial" appears at p. 2,
  p. 11 and again at p. 48 (Appendix B) — three statements of one point.
- The Paradoxical Question \* is stated in full **twice** (pp. 8 and 42).
- The disclaimer that the systems are "awkward" because they drop Multiplication
  (and sometimes Addition) totality appears at pp. 8–9, p. 36 and p. 43.
- The definable-cut literature survey appears at pp. 5–7 and again at pp. 44–47.
- `Willard2005` and `Willard2011` **delegate** rather than replicate: the
  arithmetization (Appendices B–D, Table I) and the tangibility predicates are
  cited by them, not restated. This is the corpus's largest single block of
  non-replicated machinery, and the codified statement must carry it once.

## 7. Discrepancies and errata

1. **Bibliography anachronism** (see §1a): ref. **[45]** cites a 2006 paper in a
   2001 article; ref. **[44]** still forward-references the JSL version as
   forthcoming. → drift **D22**, gap **G15**.
2. **`<` versus `≤` in the Group-3 encodings.** The pseudo-formula prints
   `y^λ < z/x` at Eq. (89) and Eq. (95), while its instantiation prints
   `y^λ ≤ z/x` at Eq. (90) and Eq. (96). The two are meant to differ only by the
   `♣ ↦ Ψ` substitution. **Image-verified on pp. 54 and 60** — a genuine
   repeated typo, not a text-layer artifact. Mathematically benign; recorded so
   the codified statement fixes one form.
3. **"Third Kurt Gödel Colloquium"** in ref. **[43]**: LNCS 1289 (1997) is the
   **Fifth** Kurt Gödel Colloquium. Ref. [40]'s "Third Kurt Gödel Symposium"
   (LNCS 713, 1993) is correct, so [43] appears to have inherited the wrong
   ordinal. Our `paperlist` has it right.
4. **Ref. [42]** is dated "December 1997" here; our `paperlist` records January
   1998 for the DIMACS volume. Minor; not actioned.
5. **"Remark 5.5's ISMULT formalism"** (p. 41) — `ISMULT` is defined in **Remark
   4.5**, not 5.5.
6. **"Theorem 6.2"** (p. 36) — 6.2 is a **Corollary**.
7. **"Lemmas C.1 and C.2"** (p. 60) — both are **Theorems**.
8. **Eq. (91)**'s informal `ISREF(A)` Group-3 carries a stray third argument
   `g` in `HilbPrf_{ISREF(A)}(⌜Ψ⌝, y, g)` and uses `ConSize(x,y)` where §2's
   Eq. (10) used `Size(y) ≤ x−1`. The `g` slot belongs to the pseudo-formula
   (92), not to the informal form.
9. **Addition-totality is printed three ways** — Eq. (6) `∀x∀y∃z z−x=y`,
   Eq. (17)/(32) `∀x∀y∃z x=z−y`, Eq. (50) as the Secondary deduction's parent.
   Same axiom; noted so a reader does not hunt for a difference.
10. **`TransProof_A(x,y)` is called a `Δ₀` formula** (p. 10) where the
    surrounding text uses `Δ⁻₀` throughout. Probably a slip for `Δ⁻₀`.
11. **Eq. (71) weakens the Group-3 consequent** from Eq. (9)'s `Ψ^x_z` to
    `Θ^x`, without comment. Not an error — the proof of Theorem 7.3 goes
    through, and the weakening strengthens the theorem — but it means
    `XIS^λ(PAX)` is not the literal "IS^λ(PAX) plus multiplication" its
    definition line suggests. Flag at C13 so the negative result is not
    misread as applying to a *stronger* schema than it does.

## 8. Saturation record

| Pass | Date | Method | New items |
| --- | --- | --- | --- |
| 1 | 2026-08-26 | Full read of all 67 pages from the text layer; smallcaps-aware awk sweep for numbered items | 41 numbered items + 2 tables |
| 2 | 2026-08-26 | Visual control pass, `pdftoppm -r 130 -png`, pp. 9, 10, 19, 22, 28, 51, 54, 60 | 0 new items; **3 text-layer corrections** (`1/3` mis-rendered as `31`; Group-Zero overbars dropped; ϖ rendered as `$`); `<`/`≤` erratum confirmed as genuine |

Zero-new-items re-pass **not yet run** — saturation is **open** pending a
third pass (charter criterion A1).

## M1 exclusions

| Label | Reason |
| --- | --- |
| Lemma 3.1a | Internal case label within Lemma 3.1 |
| Lemma 3.1b | Internal case label within Lemma 3.1 |
| Lemma 7.1b | Internal case label within Lemma 7.1 |
| Theorem 44 | Bibliographic / citation noise in the text layer, not a 2001 theorem |
| Theorem 5.7 | Cross-reference into another numbering; not a free-standing 2001 inventory header |
| Theorem 6.2 | Cross-reference / citation, not a free-standing 2001 inventory header |
