# Extraction: Willard2006a

> Tier A extraction (component C9). Template: charter ADR-0001 §Decision.
> **Anchoring**: PDF page = printed page of the author's copy (1–45); APAL
> pagination is 141 (2006) pp. 472–496 and is not used here.

## 1. Identity and witness

| Field | Value |
| --- | --- |
| Corpus key | `Willard2006a` |
| Title | A Generalization of the Second Incompleteness Theorem and Some Exceptions to It |
| Venue | Annals of Pure and Applied Logic 141(3), 2006, pp. 472–496 |
| Witness | `nachlass/papers/willard2006_generalization_second_incompleteness_author_apal6.pdf` (45 pp.) |
| Structure | §1 Introduction (1–9); §2 Literature survey (9–16); §3–4 `ISCE(A)` (16–22); §5 Concise Encoding and Theorem 4 (22–31); §6 `ISINF(A)` (31–36); Appendices A–D (36–45) |

## 2. Role in corpus

The **Hilbert-apparatus** counterpart to the tableaux line, and the paper that
introduces a boundary axis found nowhere else: the **naming convention** for
constant symbols. It is also the item whose subsumption by `Willard2011` was
deferred from C6 — see §7.

## 3. The naming-convention axis — and the corpus's unifying device

Constant symbols `C₁, C₂, C₃, …` may be defined by one of three conventions
(pp. 2–3); integers without names are built from named ones by subtraction and
division (so `5 = C₄ − C₂ − C₁` additively):

| Eq. | Convention | Recurrence |
| --- | --- | --- |
| (1) | incremental | `C_i = C_{i−1} + 1` |
| (2) | **additive** | `C_i = C_{i−1} + C_{i−1}` (doubling) |
| (3) | **multiplicative** | `C_i = C_{i−1} * C_{i−1}` (squaring) |

**These are the corpus's two recurring sequences, here elevated to a design
axis.** The very same doubling/squaring pair drives `Willard2001` Remark 4.5's
footnote 7 (`u_{i+1} = u_i²` breaks the positive engine), `Willard2002a` Lemma
4.7 (`u_{i+1} = (u_i)²` powers the negative theorem), and `Willard2020` §5
(`xᵢ` doubling vs `yᵢ` squaring as the growth intuition). One device, four
papers, both sides of the boundary. Obligation **O44**.

**Continuously Expanding**: a sequence of axioms defining `C₁, C₂, …` is
continuously expanding iff there are `K₁ < K₂ < …` such that the axioms with
Gödel numbers `< K_i` suffice to prove the existence of an integer `> K_{i+1}`.
The additive and multiplicative conventions qualify (given `O(Log(i+2))`-bit
axioms); **the incremental convention does not — "it grows too slowly"**.

This is why the additive convention is the sweet spot:

> The additive convention will thus represent a useful compromise between the
> faster-growing multiplicative convention and the slower incremental
> convention — whose growth rate is simultaneously sufficiently **slow** to
> satisfy Theorem 3's self-justification property while also sufficiently
> **fast** to satisfy the continuous expansion property.

And Willard says explicitly that the incremental-convention version of Theorem 3
is **`Willard2001`'s Theorem 3.4** (`ISREF(A)`'s consistency preservation) — so
`ISCE(A)` is precisely `ISREF(A)` upgraded from incremental to additive naming,
buying continuous expansion. Obligation **O47**.

## 4. Language

**Seven** Grounding Functions (image-verified, p. 6): Subtraction (`x−y=0` when
`x<y`), Division-with-rounding, `Maximum(x,y)`, **`Logarithm(x) = 1 + ⌊Log₂x⌋`**,
`Predecessor(x) = Max(x−1,0)`, `Root(x,y) = ⌈x^{1/y}⌉`, `Count(x,j)`.

**The Non-Growth condition itself is different here**: `F(a₁,…,a_j) ≤
Maximum(**2**, a₁,…,a_j)` — every other paper in the corpus omits the `2`.
Drift **D35**.

Classes are `Δ⁻₀`, `Π⁻₁` (as `Willard2001`), not the starred forms.

## 5. Numbered-item inventory

| Label | p. | Digest | Proof |
| --- | --- | --- | --- |
| Theorem 1 | 4 | **(Pudlák 1985)** For consistent `α ⊃ Q` and any definable cut `ϕ`, α can prove neither its Hilbert consistency nor the weaker `∀p ϕ(p) ⇒ ¬Contra_α(p)` | cited |
| Theorem 2 | 4 | **(Solovay's 1994 modification of Pudlák, using Nelson and Wilkie–Paris)** A consistent β canonically formalising arithmetic, proving successor totality (5) and the Π₁ associativity/commutativity/distributivity/identity theorems for `A`/`M`, cannot prove the non-existence of a Hilbert proof of `0=1` from itself | cited (App. of `Willard2001`) |
| **Theorem 3** | 6 | For consistent `A` with all `Π⁻₁` theorems standard-model-valid and `HilbPrf_A` `Δ⁻₀`-encodable, there is a consistent **`ISCE(A)`** using the **additive** naming convention that (i) proves all `A`'s `Π⁻₁` theorems and (ii) recognises the non-existence of a **Hilbert-style** proof of `0=1` from itself | full (§4) |
| **Theorem 4** | 7 | No consistent α can prove its own Hilbert consistency when it (1) contains all the **multiplicative** naming convention's axioms, (2) proves all PA's `Π⁻₁` theorems, and (3) has the **Concise Encoding** property (Definition 1, §5) | full (§5) |
| Remark 1 / **Theorem 4\*** | 7, 40 | Drops Concise Encoding and isolates a `Π⁻₁` theorem `W` of PA such that no consistent r.e. `α ⊃ W` using the multiplicative convention can verify its own Hilbert consistency | sketch (Appendix C) |
| Lemma 1 | 17 | A `Δ⁻₀` encoding result underlying `ISCE(A)` | full |
| Remark 2 | 19 | On the `Δ⁻₀` assignment to `HilbPrf_{ISCE(A)}(x,y)` | n/a |
| Theorem 3\* | 22 | Strengthened form of Theorem 3 | full |
| Definition 1 | 23 | **Concise Encoding** property (with the constraint `q < 2^p`) | n/a |
| Lemma 2 | 23 | For α proving all PA's `Π⁻₁` theorems | full |
| Remark 3 | 24 | n/a | n/a |
| Definition 2 | 24 | An encoding of `N` using only the constant symbols and the overbrace notation | n/a |
| Lemma 3 | 25 | The bit-length of `N`'s encoding is `O[(…)]` | full |
| Definition 3 | 25 | `Log^λ z` — the λ-fold iterated logarithm | n/a |
| Lemma 4 / Corollary 1 | 27 | Sentence (34) is a theorem of Peano Arithmetic | full |
| Lemma 5 | 27 | For consistent α satisfying Lemma 4's hypothesis | full |
| Theorem 5 | 28 | On `ShortPrf^λ_α(x,y,z)` and Definition 3's `Subst*(g,h)` | sketch (Appendix B) |
| **Theorem 6** | 31 | For α satisfying the §6 invariants — the **`ISINF(A)`** construction | full/sketch (§6) |
| **Theorem 7** | 34 | There is a `Π⁻₁` theorem `W` of PA such that no consistent α can simultaneously prove `W`, prove Eqs. (43)–(45), and verify its own Hilbert consistency | sketch (Appendix D, by reduction to Theorems 1 and 2) |
| Lemma 6, Lemma 7 | 39 | `Π⁻₁` sentences supporting Theorem 4\* | stated-only ("Lemma 6's proof is not provided here") |
| Definitions 4, 5 | 39–40 | Supporting definitions for Appendix C | n/a |

**`ISINF(A)` and Infinite Far-Reach.** α is **Infinitely Far-Reaching** iff some
finite `S ⊆ α` proves `∃x Pred^N(x) = 1` for every integer `N`. §6 outlines a
system that is simultaneously (1) Infinitely Far-Reaching, (2) able to verify
its own Hilbert consistency, and (3) able to prove all PA's `Π⁻₁` theorems —
which Willard notes "one's first intuition would be" impossible given Theorems
1, 2 and 4. He concedes it is "highly awkward in its internal structure."

## 6. The `Willard2011` subsumption check (deferred from C6)

`Willard2011` claims to unify `[59, 64, 66, 68]` = `Willard1993`, `Willard2005`,
**`Willard2006a`**, `Willard2009`. What 2006a actually contributes, and whether
2011 carries it:

| 2006a's content | Carried by `Willard2011`? |
| --- | --- |
| `ISCE(A)`: a **Hilbert-apparatus** self-justifying system | **Yes** — 2011's `SJAS-NoAddition-Hilbert` configuration |
| The **naming-convention axis** (incremental / additive / multiplicative) | **No.** 2011's generic configuration ξ parameterises language, base and coding `g`, but has no component for how constant symbols are *named*, and the Continuous Expansion property has no 2011 counterpart |
| **Continuously Expanding** as a desideratum | **No** |
| **Infinitely Far-Reaching** / `ISINF(A)` | **No** |
| Theorem 4/4\*/7's Hilbert negative results | **Partially** — 2011 cites the negative line but does not reproduce Theorem 7 |

**Verdict: the subsumption is partial.** 2011 absorbs 2006a's *positive system*
but drops its *boundary axis* entirely. This is the second element-wise failure
of 2011's unification claim, alongside D21's paradigm/configuration mismatch.
Recorded as drift **D36**; extends the C6 audit.

## 7. Discrepancies and errata

1. **Definable Cut, third definition.** Eq. (4) here is `ϕ(0) ∧ ∀x ϕ(x) ⇒
   ϕ(x+1) ∧ ∀x∀y<x ϕ(x) ⇒ ϕ(y)` — three clauses. `Willard2001` p. 5 uses
   `Υ(k̄)` for each fixed `k` plus downward and successor closure;
   `Willard2002a` Eq. (1) uses only `Υ(0)` and successor closure. Drift **D32**.
2. **Non-growth is defined with `Maximum(2, …)`** here alone. Drift **D35**.
3. **`Logarithm(x) = 1 + ⌊Log₂x⌋`** is a fourth variant across the corpus
   (`Willard2001`/`Willard2020`: `⌈Log₂(x+1)⌉`; `Willard2005`: `⌊Log₂x⌋`).
   Extends **D23**.

## 8. Saturation record

| Pass | Date | Method | New items |
| --- | --- | --- | --- |
| 1 | 2026-08-26 | Text-layer read of §§1–2 and the theorem statements; awk item sweep across all 45 pp. | 22 numbered items |
| 2 | 2026-08-26 | Visual control pass, p. 6 | 0 new; confirmed seven grounding functions, `Logarithm = 1+⌊Log₂x⌋`, `Root = ⌈x^{1/y}⌉`, and non-growth with `Maximum(2, …)` |

§§3–6 and Appendices A–D were swept but **not read line by line**; several proof
statuses above are provisional. Gap **G23**. Saturation **open**.
