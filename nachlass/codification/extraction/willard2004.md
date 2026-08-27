# Extraction: Willard2004

> Tier A extraction (component C9). Template: charter ADR-0001 §Decision.
> **Anchoring**: PDF page *n* = printed page *n + 336* (PDF 4 = printed 340).
> Anchors below are **PDF** pages.

## 1. Identity and witness

| Field | Value |
| --- | --- |
| Corpus key | `Willard2004` |
| Title | A Version of the Second Incompleteness Theorem for Axiom Systems that Recognize Addition but not Multiplication as a Total Function |
| Venue | *First Order Logic Revisited* (eds. Hendricks, Neuhaus, Pedersen, Scheffler, Wansing), Logos Verlag Berlin, 2004, pp. 337–368 |
| Witness | `nachlass/papers/willard2004_addition_not_multiplication_fol75.pdf` (32 pp.) |
| Structure | §1–2 Introduction and notation (1–9); §3 Structure of Theorem 1's proof (9–15); §4–5 Lemmas and Theorem 2 (16–25); §6 and appendix (25–32) |

## 2. Role in corpus

`Willard2002a` settled the multiplication-total case. This paper settles the
**Type-A** case — systems recognising Addition but *not* Multiplication as
total, which is exactly where `Willard2001`/`Willard2005`'s positive results
live. It is therefore the negative result that bounds the corpus's own
affirmative systems, and it is the source `Willard2020` §7 cites for "the
evasions fail at and above the Π\*₂ level".

## 3. The Level hierarchy — the corpus's canonical definition

This paper supplies the definitions the rest of the corpus uses loosely
(pp. 5–6):

- **Level(n)**: no `Π*ₙ` sentence `Υ` has simultaneous tableaux proofs of `Υ`
  and `¬Υ`.
- **Level(n+)**: no `Q*ₙ` sentence does, where a **`Q*ₙ` sentence** is a `Σ*ₙ`,
  a `Π*ₙ`, or any Boolean combination of such.
- **Level(0-)**: there is no proof of `0=1` from α.

> All definitions of consistency, from Level(0-) up to Level(n+) for any `n`,
> are equivalent to each other under strong enough models of Arithmetic.
> However, **many weak axiom systems do not have a mathematical strength to
> formally prove and recognize this equivalence.**

That last sentence is why the level index is a real axis and not bookkeeping.
Obligation **O45**.

## 4. Language and encoding

Six **Grounding Functions**: Subtraction (`x−y = 0` when `x ≤ y`), Division,
`Maximum`, `Logarithm(x)`, `Root(x,y) = ⌈x^{1/y}⌉`, `Count(x,j)` — the same six
as `Willard2020`, with **no Predecessor**. **U-Grounding** adds Addition and
`Double(x) = x + x`, giving eight.

**U-Grounded Binary Representation `N̲`**: every `N ≥ 2` is expressible using at
most **`2·Log₂N`** applications of Addition and Double to the constants `0` and
`1` — e.g. `25 = 1 + Double(Double(Double(1 + Double(1))))`. Compare
`Willard2020`'s `3·⌈Log n⌉` and `Willard2002a`'s `O(Log N)`: one device, three
constants, differing by what is counted.

Willard notes the eight-symbol language is "technically unnecessary" — the six
non-growth functions plus Eq. (2) `∀x∀y∃z x = z−y` would do — but shortens the
proofs.

## 5. `Tab-ℜ-List` deduction — the origin of 2020's `Tab-1`

A **`Tab-ℜ-List` proof** of `T` from α is a sequence `(t₁,p₁),…,(t_n,p_n)` with
`T = t_n`, where each `p_i` is a tableaux proof of `t_i` whose axioms come from
α or from `t₁,…,t_{i−1}`, and every intermediate `t_i` lies in the prespecified
class **ℜ**.

`Tab₁List` abbreviates the case `ℜ = Π*₁ ∪ Σ*₁`. **This is exactly
`Willard2020`'s `Tab-1`**, whose `Rank-1*` constraint is the same condition
under a later name. Recorded as drift **D33**.

Willard relates `Tab-Q*_k-List` to the **R-proofs and `Q_k`-style proofs** of
Hájek, Paris, Pudlák and Wilkie, and states the essential difference:

> the R-proofs and `Q_k` style proofs of [7, 16, 29] are based on **partially
> limiting** the power of Hilbert-style deduction, whereas our dual form of this
> construct proceeds in the opposite direction — where we seek to progressively
> **expand** the logical power of Semantic Tableaux style deduction instead.

## 6. Numbered-item inventory

| Label | p. | Digest | Proof |
| --- | --- | --- | --- |
| **Theorem 1** | 6 | There is a `Π*₁` theorem `W` of PA such that no consistent **finite** `α ⊃ W` in the U-Grounding language (hence Addition total) can recognise its own **Level(2+)** tableaux consistency | full (§§3–4) |
| Remark 1 | 6 | Theorem 1 extends to infinite α satisfying the **Conventional Deciphering Property**, here defined as: a `Σ*₀` sentence `Test(n)` holds exactly when `n` is the Gödel number of an axiom of α. "We will not have the page space to prove this stronger result here" | stated-only |
| Remark 2 | 6 | Distinguishes this from `Willard2002a`: that paper needed Addition **and** Multiplication total; this one needs only Addition. Since `Willard2002c` shows Level(0-)–Level(1) evasions exist for multiplication-as-relation systems, **Level(2+) is where G2 returns for Type-A systems** | n/a |
| **Theorem 2** | 8 | There are two `Π*₁` theorems of PA, `V_A` and `V_B`, with **(A)** no consistent `α ⊃ V_A` proving its Level(0-) consistency under **`Tab-Π*₂-List`** deduction, and **(B)** no consistent `α ⊃ V_B` doing so under **`Tab-Σ*₂-List`** | full (§§4–5) |
| **Theorem 3** | 8 | For `Tab₁List` (`ℜ = Π*₁ ∪ Σ*₁`): for every consistent `A ⊇ PA` there is a consistent α that (1) recognises its own **Level(1)** consistency under `Tab₁List`, (2) proves all `A`'s `Π*₁` theorems, (3) recognises Addition as total | sketch (3-page appendix outlining how `Willard2002c`'s proof strengthens) |
| Theorem 4 | 11 | A further negative result for α proving all PA's `Π*₁` theorems | full |
| Theorem 5 | 23 | The `Tab-ℜ-List` generalisation for arbitrary sentence classes `ℜ` | full |
| Lemma 1 | 16 | `W₀`–`W₇` are `Π*₁` theorems of Peano Arithmetic | full |
| Lemma 2 | 16 | Properties of `Υ_i`, `Θ_m`, `0_n` from Eqs. (6)ff. | full |
| Lemma 3 | 17 | Existence of a constant `K₃ > 0` bounding a proof length | full |
| Lemma 4 | 18 | Existence of a constant `K₄ > 0` with a per-`n` bound | full |
| Lemma 5 | 19 | For finite α extending the base, a proof-transformation invariant | full |
| Lemma 6 | 21 | `MinAx(α)`, a `Σ*₀` formula identifying α as minimal | full |
| Lemma 7 | 23 | `MinW(α)`, the analogue for `W` | full |

## 7. The complementarity Willard draws (p. 9)

> Theorem 3 established that there exists a **Boundary-Case exception** to the
> Second Incompleteness Theorem when `ℜ` represents the union of `Π*₁` and
> `Σ*₁` sentences, while **Theorem 2 shows the Second Incompleteness Theorem
> comes to force** when `ℜ` represents instead either the class of `Π*₂`
> sentences or the class of `Σ*₂` sentences. Moreover, Theorem 3 indicates that
> its Boundary-Case exception rises up to **Level(1)** definitions of
> consistency, while Theorem 2 shows that even the lower **Level(0−)** is
> problematic under `Tab-Π*₂-List` and `Tab-Σ*₂-List` deduction.

**This materially sharpens `Willard2020` §7's conjecture.** 2020 parameterises
by `Z` — which *LEM instances* are logical axioms — and reports Δ\*₀ surviving,
Π\*₂ failing, Π\*₁ open. 2004 parameterises by `ℜ` — which *intermediate
theorems* a TabList proof may use — and here the `Π*₁ ∪ Σ*₁` case is **proved**
(Theorem 3), not conjectured. So the two dials are different, and 2020's open
conjecture is precisely the claim that **2004's proved positive result transfers
from the `ℜ` dial to the `Z` dial**. Recorded as drift **D34**, obligation
**O46**, and appended to gap **G22**.

## 8. Discrepancies and errata

1. **"Conventional Deciphering Property" is redefined.** `Willard2002a`
   Definition 5.1 requires a *finite subset* `F ⊆ α` proving axiomhood of each
   canonical binary term; here (Remark 1) it is a `Σ*₀` sentence `Test(n)`
   deciding axiomhood. Same name, different condition — drift **D31**.
2. The encoding is delegated to "page 581 of our article [32]" = `Willard2001`,
   i.e. its Appendix B — confirming the delegation chain recorded at C7.
3. Theorem 3 is credited to the last page of `Willard2002c` (TABLEAUX 2002),
   whose printed proof covered only the plain-tableaux special case; the
   appendix here outlines the strengthening. `Willard2002c` is a C10 item.

## 9. Saturation record

| Pass | Date | Method | New items |
| --- | --- | --- | --- |
| 1 | 2026-08-26 | Full text-layer read of §§1–3 and item sweep across all 32 pp. | 13 numbered items |
| 2 | 2026-08-26 | Visual control pass, `pdftoppm -r 125 -png`, p. 4 | 0 new; confirmed the six grounding functions, `Root = ⌈x^{1/y}⌉`, non-growth as `≤ Maximum(a₁,…,a_j)` (no `2`), `2·Log₂N`, and Eq. (2) |

Zero-new-items re-pass **not yet run**; saturation **open**. Only §§1–3 were read
in full; §§4–6's proof detail was swept for numbered items but not read line by
line — recorded as gap **G23**.
