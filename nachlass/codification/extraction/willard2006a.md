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
| Definition 1 | 23 | **Concise Encoding** property: `HilbPrf_α(x,y)` is `Δ⁻₀` **and** for some constant `R > 0` and finite `F ⊂ α`, (I) any proof `p` of `t` from `α` has **`∃q < 2^{p^R}`** an `F`-proof of Eq. (31), and (II) PA proves `(α,F,R)` satisfies (I). **Image-verified 2026-09-01 (Q21)** — previously recorded as `q < 2^p`, dropping the exponent `R`. Willard: the bound "is somewhat excessive because `q` will typically have a much smaller magnitude" | n/a |
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

| 3 | 2026-08-27 | Completion read of pp. 1–2 and 9–45 | **6 new items** (Theorem 3\*, Remarks 1–3, Corollary 1, Definitions 4–5, plus Hybrid(H) and Pudlák's question); **four proof-status corrections** |

**Coverage complete** (pp. 1–45). Zero-new-items re-pass **not yet run**.

### Added at the 2026-08-27 completion read

- **`ISCE` = "Introspective Semantics with Continuous Expansion"** (p. 16).
  Group-1 delegates explicitly to "**Table I of [46]**" = `Willard2001`, and
  Lemma 1's encoding to "Appendixes B through D of our article [46]" —
  confirming the C7 delegation chain from the far side.
- **Formal naming conventions** (pp. 13–14) replacing the informal Eqs. (1)–(3),
  with `ADD`/`MULT` as Δ⁻₀ predicates, plus `Bit(x,i) = Count(x,i) − Count(x,i−1)`,
  `σ_d(x)`, and **2-reduced** Π⁻₁ sentences — the devices that let `ISCE(A)`
  work without a constant symbol for every natural.
- **Remark 3 (p. 24) is the crux of the paper** and the cleanest statement of
  the growth restriction in the corpus — see obligation **O50**.
- **Hybrid(H) naming** (p. 30): `C_i = ⌈2^{[Log i]^H}⌉·C_{i−1}`, with the
  positive result surviving at **H = 1** and the negative results generalising
  for **H > 1**. The corpus's only exactly-located threshold on the **Hilbert**
  side (D38). *Corrected 2026-09-04:* it is not the only exactly-located
  threshold outright — `Herb−k` (`k = 1`/`k = 2`) and `Willard2002c`'s `R(i,j)`
  (`R(1,1)`/`R(2,1)`, D46) state theirs exactly too, as D38's 2026-08-27
  amendment records.
- **`ISINF(A)`'s Group-3 has two axioms** (41) and (42), the latter an
  **Expansion Axiom**, defined simultaneously by the Fixed Point Theorem; the
  trick is `JumpPrf_α(x,y)`, which is Δ⁻₀ **even though α recognises neither
  successor nor doubling as total**. Theorem 6's Item 1 is only sketched.
  `ISINF(A)` "appears to be **incompatible** with the tangibility reflection
  principles", unlike `ISCE(A)` — so the Hilbert-side positives split, one
  keeping reflection and one trading it for Infinite Far Reach.
- **Pudlák's open question** (pp. 33–34), gap **G27** — see §5's inventory.
- **Appendix D**'s obstacle and its resolution: a non-standard model with
  `F(x) = x+1` on standard `x` and `F(x) = x−1` elsewhere is consistent with
  (43)–(45) yet slower-growing than successor; `Ψ(x) = ∀t≤x F(t) > t` is shown
  to satisfy the three cut conditions, making successor total **locally** within
  `Ψ` and the Pudlák–Solovay machinery applicable.
- **Appendix A corroborates D25/O33 from a fourth paper**: a byte is six bits,
  proofs are base-64 integers, **20** symbols with 6-bit codes, variables in
  `⌈log₃₂(i+1)⌉ + 1` bytes — identical to `Willard2001` Appendix B but for 20
  symbols rather than 21, the missing one being the tableaux parameter marker.
- **But compression is disclaimed as necessary here** (pp. 15, 37): the results
  "generalize in various forms for non-compressed encodings" and are merely
  "substantially more meaningful" with compression. Drift **D41**, obligation **O51**.
- **Proof statuses corrected**: Theorem 3\* → `stated-only`, Corollary 1 →
  `sketch`, Lemma 3 → `sketch`, Theorem 5 → `cited`. Verified: Theorems 3, 4
  `full`; Lemmas 4, 5 `full`; Lemmas 6, 7 `stated-only`.
- The literature survey (§2, 11 items) is the best related-work map in the
  corpus, and item 6 identifies **Solovay's never-published thinning
  construction, proved on Hájek–Pudlák pp. 172–173** — which is exactly the
  citation `Willard2020` gives for statement ⊙, completing obligation O48's chain.

## M1 exclusions

| Label | Reason |
| --- | --- |
| Theorem 2.3 | Citation of Pudlák / prior literature, not a 2006a numbered item |
| Theorem 3.4 | Citation of Willard2001 Theorem 3.4, not a 2006a numbered item |
