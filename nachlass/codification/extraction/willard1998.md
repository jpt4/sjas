# Extraction: Willard1998

> Tier C extraction (component C11). Template: charter ADR-0001 §Decision.
> **Anchoring**: the witness is the **DIMACS volume** (`Dimacs1996.pdf`, 170
> pages, image-only, two-page spreads). Printed chapter pages **297–320** sit on
> PDF pages **158–170** (title on PDF 158 right = printed 297; last references
> on PDF 170 left = printed 320). All formula anchors below use **printed**
> page numbers; PDF spread numbers appear in §8. OCR text
> (`../sources-text/willard1998.txt`) is a **search aid only** — page images
> govern (gap G10 closed as chapter-only OCR aid; full-volume collation not
> required once this chapter is extracted).
>
> **Relation to later papers**: this is the **DIMACS reflection / NP** paper
> (prehistory of SJAS). `Willard2001` cites **`Willard1997`** (KGC5 / LNCS 1289)
> for ISTR proofs, **not** this 1998 piece. Do not conflate the two (gap G2 /
> G17 remain for Willard1997).

## 1. Identity and witness

| Field | Value |
| --- | --- |
| Corpus key | `Willard1998` |
| Title | Self-Reflection Principles and NP-Hardness |
| Venue | DIMACS Series in Discrete Mathematics and Theoretical Computer Science, Volume 39 (January 1998), pp. 297–320 |
| Witness | `nachlass/papers/willard1998_self_reflection_np_hardness_dimacs_volume.pdf` → `lit/Dimacs1996.pdf` (170 pp., volume containing the chapter; image-only, no text layer) |
| Funding | NSF Grant CCR 93-02920 |
| MSC 1991 | 68QXX, 03FXX, 03BXX |
| Text aid | `../sources-text/willard1998.txt` (chapter OCR; search aid) |
| Structure | §1 Introduction (297–299); §2 Tangibility Reflection Principles (299–301); §3 Definition of ISREF(A) (301–304); §4 Consistency Preservation (304–305); §5 Generalizations of Theorem 4.1 (305–306); §6 A New Variation of the Second Incompleteness Theorem (306–308); §7 Self Justification and NP (308–309); §8 Intuition Behind the Cognitive Conjecture (309–310); §9 The P=?NP Question (310–314); §10 Further Results (314–316); Acknowledgments (316); Appendix The Gödel Encoding of Group-3 Axioms (316–319); References (319–320) |

## 2. Role in corpus

**Prehistory of the reflection line.** The paper’s dual goals (abstract, p. 297):

1. Outline the capacities of weak axiom systems that can prove their own
   consistency (**self-justifying** systems).
2. Explain how properties of those systems relate to open questions about **NP**.

It is the first published place in the witnessed corpus that:

- states the two-clause **Self-Justifying** definition;
- introduces **TangPred / TangDiv_k / TangRoot_k** and the **Tangibility
  Reflection Principle** (Eq. 2);
- defines **ISREF(A)** with the four axiom groups and a **4-tape UTM**
  simulation language;
- links the reflection apparatus to **P=?NP** via `ISREF^R`, the Cognitive
  Conjecture, and Theorems 9.5–9.6;
- systematically records the cost: dropping totality of **Multiplication**,
  and often of **Addition** and **Successor**.

`Willard2001` expands the ISREF / tangibility mathematics into full journal
form and adds the tableaux / `IS^λ` line; this 1998 piece is the DIMACS
**reflection+NP** statement. `Willard2001` Remark 3.8’s ISTR proofs go to
`Willard1997` [43], not here. Resume (DEW-Resume A.11) lists this paper among
the journal publications of the SJAS program.

## 3. Systems and machinery defined

### 3.1 Self-Justifying (two conditions)

An axiom system `α` is **Self-Justifying** (p. 297) iff:

1. one of the theorems implied by `α` is a statement of its own consistency
   (under some reasonable definition of self-consistency);
2. `α` is in fact consistent.

Kleene’s Fixed Point Theorem lets one build `α*` satisfying (i) by adding
“there is no proof of 0=1 from the union of `α` with this sentence”; the catch
is that `α*` can still violate (ii). No system satisfying the Hilbert–Bernays
derivability conditions can be self-verifying.

### 3.2 Notation and the Canonical Reflection Principle

- Subtraction `x ∸ y = 0` when `x < y`; Division `[x/y] = x` when `y = 0`.
- `⌈Φ⌉` = Gödel number of `Φ`; `Prf_α(x,y)` is a `Δ₀` formula saying `y` codes
  an `α`-proof of the sentence with Gödel number `x`.
- **Canonical Reflection Principle** (Eq. 1, p. 298):

```
{ ∃y Prf_α(⌈Ψ⌉, y) } ⊃ Ψ
```

Generalizations of Gödel’s Second Incompleteness Theorem (Pudlák / Solovay
line, points A–C, pp. 298–299) show that even systems recognizing *none* of
Successor, Addition, or Multiplication as total still cannot prove (1) for
every `Π₁` sentence if they prove all of PA’s `Π₁` theorems about Subtraction
and Division.

Quantifier bounding (p. 299): `Φ^i_j` is `Φ` with unbounded universal
variables bounded by `i` and unbounded existential variables by `j`;
`Φ^†` abbreviates `Φ^L_∞`.

### 3.3 Tangibility predicates and Eq. (2)

Three definitions of tangibility (`k ≥ 2` for the last two; **image-verified**
p. 300):

```
TangPred(x)     ≡  ∃z  x = z − 1
TangDiv_k(x)    ≡  ∃z  x < z / k
TangRoot_k(x)   ≡  ∃z  x < z^{1/k}
```

**Drift D79**: `Willard2001` Definition 1.2 prints `TangPred(x) ≡ ∃v x < v − 1`
(strictly stronger / differently phrased). Do not silently unify the two
printings.

**Tangibility Reflection Principle** for `Ψ` (Eq. 2, p. 300):

```
∀x { [ ∃y Prf_α(⌈Ψ⌉, y) ∧ Tangible(x) ] ⊃ Ψ^x }
```

where `Tangible` is any of the three predicates and `Ψ^x` relativizes
quantifiers to bound `x`.

The intended epistemic reading (Paradoxical Question (*), p. 300): a Thinking
Being may assume that proofs of `Ψ` are valid when restricted to numbers of
“reasonable size” (tangible numbers), even though the full Canonical
Reflection Principle (1) yields inconsistency.

**Cost** (p. 300–301): self-justifying systems supporting (2) must drop
Multiplication-totality; many also drop Addition- and Successor-totality.
These are the **ISREF** systems.

### 3.4 ISREF(A): language and four axiom groups

**“IS”** = Introspective Semantics; **“ISREF”** = Introspective Semantics with
Reflection (p. 302).

**4-tape UTM `M`** (pp. 301–302): left-sided tapes (infinite to the left;
address 0 at the right); alphabet `{0,1,2}` (bits plus null); tapes
`T_E, T_1, T_2, T_3`. Simulation functions:

- `Tape_i(Z, x, t)` — symbol at position `x` on tape `i` at time `t`;
- `Head_i(Z, t)` — head address;
- `State_i(Z, t) = 1` iff machine is in state `i` at time `t`.

**Non-Growth Function** (p. 302): `F(a₁,…,a_j) ≤ Maximum(2, a₁,…,a_j)` for all
arguments. (**Drift**: `Willard2001` omits the constant `2` in the Maximum.)

**Eight Grounding Functions** (non-Turing; p. 302; image-verified):

| # | Function | Definition |
| --- | --- | --- |
| 1 | `Maximum(x,y)` | — |
| 2 | `Logarithm(x)` | `⌈Log₂(x+1)⌉` |
| 3 | `Predecessor(x)` | `Max(x−1, 0)` |
| 4 | `Root(x,y)` | `⌈x^{1/y}⌉` (**ceiling**; 2001 uses floor — D80) |
| 5 | `Count(x,j)` | number of `1`-bits among `x`’s rightmost `j` bits |
| 6 | `Bit(x,y)` | `Count(x,y) − Count(x,y−1)` |
| 7 | Integer Subtraction | §1 |
| 8 | Integer Division | §1 |

Plus the Tape / Head / State Turing functions. Classes: `Δ₀⁺` (all quantifiers
bounded), `Π₁⁺` (`∀v₁…∀v_n Φ` with `Φ` `Δ₀⁺`), using Subtraction/Division as
primitives so growth appears only via unbounded existentials.

**Four axiom groups** (pp. 302–303):

| Group | Content |
| --- | --- |
| **Zero** | constants `n̄` for `n ≥ 0`; `1̄ ≠ 0̄`; `Predecessor(0̄)=0̄`; for `n>0`: `Predecessor(n̄)=‾(n−1)`, `‾(2n)−n̄=n̄`, `‾(2n+1)−n̄−1̄=n̄` (last two for log-compression of constant definitions; Footnote 1) |
| **1** | finite `Π₁⁺` set fixing `=` / `<` and evaluating grounding functions on constants |
| **2** | for each `Π₁⁺` sentence `Φ`: `∀y { Prf_A(⌈Φ⌉, y) ⊃ Φ }` (Eq. 3) |
| **3** | `Size(y)` = largest constant stored in proof `y` (else 0); for each prenex `Ψ`: Eq. (4) |

**Group-3 / size reflection** (Eq. 4, p. 303; image-verified):

```
∀x ∀y { Prf_ISREF(A)(⌈Ψ⌉, y) ∧ x > Size(y) ⊃ Ψ^{x−1}_{x−1} }
```

Clarifying comments (pp. 303–304): (1) ISREF recognizes **no growth
functions** — otherwise (4) would not be valid; (2) (4) is self-referential
(Appendix encoding); (3) if `A` is consistent then ISREF(A) proves all `Π₁`
theorems of PA (under the Subtraction/Division rewrite); (4) specializing to
`Ψ = "0=1"` and using `0≠1` yields flat consistency
`∀y ¬Prf_ISREF(A)(⌈0=1⌉, y)` (Eqs. 5–6).

### 3.5 Consistency preservation

- **Regularly Consistent** (p. 304): `A` is sound, and `Prf_A` is encoded as a
  `Δ₀⁺` formula.
- **Consistency-Preserving**: `I(•)` maps regularly consistent `A` to a
  consistent `I(A)`.
- Conventional sizing (Eq. 7, p. 305): `Size(p) ≤ p − 2`.

**Theorem 4.1** (p. 305): if `A` is regularly consistent then `ISREF(A)` is
consistent — so `ISREF(•)` is consistency-preserving. Proof sketch: minimal
counterexample triple `(Ψ^i_i, p, i)` against Group-3, finite model
`M_i = {0,…,i}`, descent on `i`.

### 3.6 Apparatus ↔ tangibility ↔ totality trade-off (§5)

From Eq. (4), ISREF(A) supports Tangibility Reflection under **TangPred**
(and, with adjustments, TangDiv / TangRoot). Strengthening Group-1 forces
weakening Group-3 (pp. 305–306):

| Extension of Group-1 | Forced Group-3 weakening | System | Tangibility supported |
| --- | --- | --- | --- |
| (none — ISREF) | Eq. (4) as stated | `ISREF(A)` | TangPred |
| **Bitwise-Or** total | weaken schema | `ISTR(A)` | TangDiv₂ (not TangPred) |
| **Addition** total | weaken to **cut-free** apparatus (Herbrand / tableaux / cut-free sequent) | `IS^λ(A)` (`λ > 0` in this paper’s wording) | TangRoot-style |

This is the same design-space table that `Willard2001` §1 results (i)–(ii)
codify; 1998 already has the three named systems. ISTR proofs are **not**
given here (later deferred in 2001 to Willard1997).

### 3.7 Negative reflection results (§6)

- **PAX**: trivial extension of PA whose function symbols include ISREF’s
  Group-1 symbols plus Addition and Multiplication (p. 306).
- **DIAGONAL(α)**: “there is no proof of this sentence from `α`”; formalized
  via `SUBST(g,h)` as Eq. (10).
- **Theorem 6.1**: if `α` includes ISREF’s Group-Zero/1 and proves
  `DIAGONAL(α)`, then `α` is inconsistent.
- **Theorem 6.2**: consistent Hilbert `α` verifying all PAX `Π₁⁺` theorems
  cannot verify every `Π₁⁺` Reflection Sentence of form Eq. (9)
  `∀y { Prf_α(⌈∀v φ(v)⌉, y) ⊃ ∀v φ(v) }`.
- **Remark 6.4**: enlarging ISREF’s Group-3 subscripts/superscripts beyond
  `x−1` (Eq. 14) destroys consistency.

### 3.8 ISREF^R, ISREF*, Cognitive Conjecture, P=?NP (§§7–9)

**OUTPUT(e,t,x)**: `Δ₀⁺` formula for the UTM’s output tape contents.
**Prf*_α(x,y)**: `y` is the *smallest* Gödel number of an `α`-proof of `x`.

**ISREF^R(A)** for nondecreasing `R(y) < y`: Group-3 of form Eq. (18)
(quantifying `e,t ≤ R(y)` and `OUTPUT`).

**Cognitive Conjecture** (p. 309): there exists `R` growing slightly faster
than `PolyLog` (e.g. `R(y) = Log(y)^{LogLogLog(y)}`) such that
`ISREF^R(PAX)` is consistent.

**ISREF*(A)**: Group-3 Eq. (19) using `TangPred(x)` and `Prf*` — patently
weaker than ISREF(A)’s Group-3; consistent whenever ISREF(A) is.

**SAT(a,b)**: algorithm `a` solves every length-`N` SAT instance in time
`< N^b`.

| Label | Digest | Proof |
| --- | --- | --- |
| Lemma 9.1 | If `P=NP` and `SAT(a,b)`, a procedure finds a minimal proof of `Θ^R = DIAGONAL(ISREF^R(PAX))` in `(Log s)^d` time | sketch |
| Lemma 9.2 | If `R ≫ PolyLog`, then (A) `SAT(a,b)` ⇒ some `Υ_c(k)` true; (B) if PA/PAX proves `SAT(a,b)` then `ISREF^R(PAX)` proves `Υ_c(k)` | sketch |
| Lemma 9.3 | For each fixed `c`, `ISREF^R(PAX) ⊢ ∀y ¬PARADOX^R(y,c)` | sketch |
| Lemma 9.4 | If `ISREF^R(PAX)` proves `Υ_c(k)` then it is inconsistent | sketch |
| **Theorem 9.5** | If `R ≫ O(PolyLog)` and `ISREF^R(PAX)` is consistent, then PA (and PAX) cannot prove any `SAT(a,b)` | sketch |
| **Theorem 9.6** | If `R ≫ O(PolyLog)` and `ISREF^R` is consistency-preserving, then **`P ≠ NP`** | sketch |

Added Comment (p. 314): 9.5–9.6 give a new approach to P=?NP, but proving
consistency of `ISREF^R(PAX)` is hard.

### 3.9 ISTM^λ and cascade-self-verification (§10)

**ISTM^λ(A)** differs from ISREF(A) in two ways (pp. 314–315):

1. **Group-Zero** reduced to `1̄≠0̄` and `Predecessor(n̄)=‾(n−1)` for each
   `n>0` (no `2n` / `2n+1` compression axioms — contrast Footnote 1’s note that
   ISREF’s consistency is unaffected by those axioms, unlike ISTM).
2. **Group-3**: multi-tape machine `M` never outputs a bit-stream coding a
   proof of `0=1` from `ISTM^λ(A)` at any time `t < p^λ` (`0 < λ < 1`).

**Firmly Prove P=NP**: `S` proves some `SAT(a,b)` for all `N ≥ 2`.
**CASCONS_α(k)**: natural bitwise concatenation of `x₁…x_k` is not an `α`-proof
of `0=1`.
**cascade-self-verifying**: `α` proves `CASCONS_α(k)` for every fixed `k`.

Results cited to manuscript [35] (“Either-Or Theorem…”, 1996):

1. either `ISTM(PAX)` is cascade-self-verifying, or PA cannot firmly prove
   P=NP;
2. either some extension `A` of PA maps to a cascade-self-verifying
   `ISTM^λ(A)`, or `P ≠ NP`.

Conjecture (p. 316): `ISTM^λ` members are *not* cascade-self-verifying
(which would settle P=?NP via the §9 theorems’ style of linkage).
ISREF / ISTR / IS^λ do not appear to be cascade-self-verifying.

### 3.10 Appendix: Group-3 Gödel encoding

- `UNION(A)` = Groups Zero ∪ 1 ∪ 2.
- `ExPrf_α(s,y,h)`: proof from `α` union the sentence with Gödel number `h`.
- `IS_−(A)`: ISREF with informal Group-3 `∀y ¬Prf(⌈0=1⌉,y)` (Eq. 23), encoded
  via `Θ(g)` / Fixed Point (Eq. 24).
- `g`-pseudo-formulas with placeholder `♣`; `ClubSet`; `ExPrf*`.
- Formal Group-3: Eqs. (25)–(27) replace `Prf_ISREF(A)` by
  `ExPrf*_UNION(A)(…, N)`.
- **Theorem A.1**: `ExPrf*_UNION(A)` is `Δ₀⁺`-encodable ⇒ Group-3 axioms are
  `Π₁⁺`. Proof via PolyLog-time RAM / 4-tape UTM simulation.
- **Remark A.2**: ISREF does **not** recognize Successor, Addition, or
  Multiplication as total — why the `Δ₀⁺` encoding preserves intended meaning.
- **Remark A.3**: stronger form using only the eight non-Turing grounding
  functions.
- **Remark A.4**: same techniques apply to `ISREF^R` and `ISREF*`.

## 4. Numbered-item inventory

Printed page anchors. Proof statuses from the paper’s own labels
(“Proof Sketch”, deferred to [35], etc.).

| Label | p. | Digest | Proof |
| --- | --- | --- | --- |
| Def. Self-Justifying | 297 | Two conditions: (i) proves own consistency; (ii) is consistent | n/a |
| Eq. (1) Canonical Reflection | 298 | `{∃y Prf_α(⌈Ψ⌉,y)} ⊃ Ψ` | n/a |
| TangPred / TangDiv_k / TangRoot_k | 300 | Three tangibility predicates (image-verified forms above) | n/a |
| Eq. (2) Tangibility Reflection | 300 | `∀x {[∃y Prf_α(⌈Ψ⌉,y) ∧ Tangible(x)] ⊃ Ψ^x}` | n/a |
| ISREF(A) Groups 0–3 | 302–303 | Four-group definition; Eq. (3) Group-2; Eq. (4) Group-3 | n/a |
| Regularly Consistent / Consistency-Preserving | 304 | Admission and mapping notions | n/a |
| Eq. (7) Size bound | 305 | `Size(p) ≤ p−2` | n/a |
| **Theorem 4.1** | 305 | Regularly consistent `A` ⇒ `ISREF(A)` consistent | sketch |
| Eq. (8) | 305 | Generic Group-3 form (same shape as Eq. 4) | n/a |
| ISTR(A) / IS^λ(A) | 306 | Totality ↔ apparatus ↔ tangibility trade-offs | stated-only |
| PAX | 306 | PA + ISREF Group-1 symbols + Add/Mult | n/a |
| DIAGONAL / SUBST / Eq. (10) | 307 | Diagonal sentence | n/a |
| **Theorem 6.1** | 307 | Proving DIAGONAL(α) ⇒ inconsistency | sketch |
| **Theorem 6.2** | 307 | Consistent Hilbert α verifying PAX Π₁⁺ cannot verify all Π₁⁺ reflection (9) | sketch |
| Remark 6.4 / Eq. (14) | 308 | Strengthening Group-3 bounds beyond x−1 ⇒ inconsistency | sketch |
| ISREF^R(A) / Eq. (18) | 308–309 | R-bounded OUTPUT Group-3 | n/a |
| Cognitive Conjecture | 309 | Some R ≫ PolyLog with ISREF^R(PAX) consistent | conj |
| ISREF*(A) / Eq. (19) | 309 | TangPred + Prf* Group-3; weaker than ISREF | n/a |
| Lemma 9.1 | 311 | P=NP+SAT(a,b) ⇒ poly-log finder for min proof of Θ^R | sketch |
| Lemma 9.2 | 311 | SAT ⇒ Υ_c(k); PA-provable SAT ⇒ ISREF^R proves Υ_c(k) | sketch |
| Lemma 9.3 | 312 | ISREF^R ⊢ ∀y ¬PARADOX^R(y,c) | sketch |
| Lemma 9.4 | 313 | Proving Υ_c(k) ⇒ ISREF^R inconsistent | sketch |
| **Theorem 9.5** | 314 | ISREF^R(PAX) consistent + R ≫ PolyLog ⇒ PA cannot prove SAT(a,b) | sketch |
| **Theorem 9.6** | 314 | ISREF^R consistency-preserving + R ≫ PolyLog ⇒ P≠NP | sketch |
| ISTM^λ(A) | 314–315 | Time-bounded Group-3; reduced Group-Zero | n/a |
| Firmly Prove P=NP / CASCONS / cascade-self-verifying | 315 | Definitions for §10 either-or results | n/a |
| Either-Or results | 315–316 | Cited to [35]; cascade-self-verifying vs P=?NP | cited |
| IS_−(A) / Eqs. (23)–(27) | 317–318 | Appendix fixed-point encoding of Group-3 | n/a |
| **Theorem A.1** | 318 | ExPrf* is Δ₀⁺ ⇒ Group-3 are Π₁⁺ | sketch |
| Remark A.2 | 318 | No Successor/Add/Mult totality | n/a |
| Remark A.3–A.4 | 318–319 | Grounding-only strengthening; applies to ISREF^R / ISREF* | n/a |

## 5. Notation table

| Symbol | Meaning | Anchor |
| --- | --- | --- |
| Self-Justifying | (i) proves own consistency; (ii) is consistent | p. 297 |
| TangPred / TangDiv_k / TangRoot_k | three tangibility predicates | p. 300 |
| Eq. (2) | Tangibility Reflection Principle | p. 300 |
| ISREF(A) | Introspective Semantics with Reflection | pp. 301–303 |
| ISTR(A) | Bitwise-Or total; TangDiv₂ reflection | p. 306 |
| IS^λ(A) | Addition total; cut-free apparatus | p. 306 |
| ISREF^R(A) | R-bounded OUTPUT Group-3 | pp. 308–309 |
| ISREF*(A) | TangPred + Prf* Group-3 | p. 309 |
| ISTM^λ(A) | time-bounded consistency Group-3 | pp. 314–315 |
| PAX | PA + grounding symbols + Add/Mult | p. 306 |
| Size(y) | largest constant in proof y | p. 303 |
| Δ₀⁺ / Π₁⁺ | bounded / universal-over-bounded classes with Sub/Div (/Turing) primitives | p. 302 |
| Prf* / OUTPUT / PARADOX^R / Υ_c(k) | NP-linkage predicates | §§7–9 |
| CASCONS_α(k) | cascade consistency | p. 315 |
| UNION(A) / ExPrf* / ♣ | Appendix encoding | pp. 316–318 |

## 6. Replicated context

- Self-justifying two-clause definition → reused throughout the corpus
  (Willard2005 “Introspectively Unified Logic”, etc.).
- Tangibility Reflection Principle Eq. (2) → `Willard2001` Eq. (2);
  `Willard2005` Remark 6 hybridizes but does not redefine.
- ISREF four groups → `Willard2001` §2 ISREF(A); later Hilbert-line papers
  (ISCE, etc.) cite 2001 rather than 1998.
- Apparatus ↔ tangibility ↔ totality table → `Willard2001` §1 (i)–(ii);
  obligation O28.
- Dropping Mult/Add/Successor totality → standing program constraint;
  Remark A.2 here; Solovay/Pudlák negatives in §1 and §6.
- P=?NP / Cognitive Conjecture / ISREF^R → **unique to this paper** in the
  witnessed corpus; later papers do not develop the Cognitive Conjecture.
- Cascade-self-verifying / ISTM → cited to unpublished [35]; not resumed in
  later journal papers extracted so far.
- 4-tape UTM simulation → ancestor of later arithmetization appendices
  (`Willard2001` App. B–D).

## 7. Discrepancies and errata

1. **TangPred printing (D79)**: 1998 `∃z x = z−1` vs 2001 `∃v x < v−1`.
2. **Root ceiling vs floor (D80)**: 1998 `⌈x^{1/y}⌉` vs 2001 floor.
3. **Non-growth Maximum(2,…)** vs 2001 Maximum without `2`.
4. **Grounding count**: 1998 lists eight including `Bit`; 2001 lists seven
   (Bit recoverable from Count).
5. **IS^λ range**: 1998 §5 writes `λ > 0` / cut-free weakening; 2001 defines
   `0.01 < λ < 1` and proves consistency only at `λ = 3/4`.
6. **Bibliographic lineage**: this paper’s refs [33]=Willard1993,
   [34]=Willard1997 (Tangibility Reflection, KGC5), [35]=1996 Either-Or
   manuscript, [36]=1997 Self-Verifying manuscript (→ Willard2001). Do not
   treat 1998 as the ISTR-proof carrier — that is [34]/G2.
7. **Volume witness**: 170-page image-only DIMACS book; chapter is 24 printed
   pages on 13 PDF spreads (158–170). G10 closed as chapter-only OCR aid.

## 8. Saturation and visual control

**Saturation**: re-pass of printed 297–320 against the inventory above found no
additional numbered theorems/lemmas beyond §4’s table; section-internal
“Clarifying Comments”, the Cognitive Conjecture, and the [35]-cited either-or
results are recorded. Appendix Eqs. (23)–(27) and Theorem A.1 included.

**Visual control**: every chapter PDF page **158–170** rendered
(`pdftoppm -r 130 -png` → `/tmp/willard1998/p-*.png`) and read as an image
(both halves of each two-page spread). Printed coverage **297–320** complete.
PDF 158 left is blank facing the title; PDF 170 right is volume back-matter
(“Selected Titles”), not part of the chapter body — chapter ends at printed
320 (left of PDF 170). Load-bearing formulas (TangPred trio, Eq. 2, Eq. 4,
Group-Zero overbars, Root ceiling, Theorem 4.1, Eqs. 18–19, 9.5–9.6, A.1–A.2)
image-verified on cropped halves where needed.

**OCR**: chapter search aid written to `sources-text/willard1998.txt`; never
used as a formal anchor.
