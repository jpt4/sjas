# Extraction: Willard2020-LFCS

> Tier A extraction (component C9), recorded as a **variant witness** against
> [`willard2020.md`](willard2020.md). **It is not a subset of the arXiv paper**
> — see §3, which is the reason this record exists at all.
> **Anchoring**: PDF page *n* = printed page *n + 267* (PDF 16 = printed 283).
> Anchors below are **PDF** pages.

## 1. Identity and witness

| Field | Value |
| --- | --- |
| Corpus key | `Willard2020-LFCS` |
| Title | On the Tender Line Separating Generalizations and Boundary-Case Exceptions for the Second Incompleteness Theorem under Semantic Tableaux Deduction |
| Venue | LFCS 2020, Springer LNCS **11972**, pp. 268–286 (conference 4–7 January 2020) |
| Witness | `nachlass/papers/willard2020_tender_line_lfcs2020_lncs11972_chapter.pdf` (19 pp.), extracted from `..._volume.pdf` |
| Relation | The **initial 19-page draft** of `Willard2020` (arXiv:2006.01057v1), per that paper's p. 20 and its ref. [57] |

## 2. Item correspondence with `Willard2020`

| `Willard2020-LFCS` | `Willard2020` |
| --- | --- |
| Definition 1 (Self-Justifying GenAC) | Definition 3.2 |
| Example 2 (`SelfRef(α,D)`) | Example 3.3 |
| Definition 2 (Type-M/A/S/NS) | Definition 3.4 |
| Remark 1 (Theorem ++) | Remark 3.5 |
| Remark 2 (other evasion routes) | Remark 3.6 |
| Definition 3 (Consistency Preserving) | Definition 4.3 |
| **Theorem 1** (`IS_{Tab-1}`, `IS_{Tab}` preserving) | Theorem 4.4 |
| **Theorem 2** (`IS_{Xtab}` fails) | Theorem 4.5 |
| Example 3 (`IS^M_{Tab}`) | Example 5.1 |
| Appendix A (tableau definition) | Appendix |
| **Appendix B, Definition 4, Theorem 3** | **absent** |
| *(absent)* | §2 Gödel/Hilbert history; §7 Z-enrichment; §8 Res/Xres; §9 Ironic Events |

## 3. What the expansion dropped — the reason this is not a mere variant

`Willard2020` is longer, but it **omits** the LFCS chapter's Appendix B, which
carries the only explanation in the corpus of *how* statement ⊙ reaches
Hájek–Pudlák's definable-cut machinery. Specifically:

**Regular axiom basis** (p. 16): α presumes all U-Grounding operations
(including Addition and Doubling) total; proves all true `Δ*₀` sentences and is
consistent; and proves a `Π*₁` theorem giving addition and multiplication, as
3-way relations, their usual associative, commutative, distributive and
identity-operator properties.

**The ω-hierarchy** (image-verified, p. 16):

```
ω₀(x) = x²          ω_{j+1}(x) = 2^{ω_j(2·Log₂(x+1))}
⟹  ω_{j+1}(x) > ω_j(x)   and   ω₁(x) ≥ x^x
```

Since `L*` admits no function symbol growing as fast as multiplication, the
`ω_j` are not primitives; a `Δ*₀` formula `ψ_j(x,y)` capturing
`ψ_j(x,y) ⇔ ω_j(x) = y` on the standard numbers is built by Hájek–Pudlák's
techniques (Eq. 10).

**Definition 4 — Locally-J-Closed.** `Φ(x)` is Locally-J-Closed relative to α
iff α proves **(A)** `Φ(0)`, `Φ(1)`, `Φ(2)`; **(B)** closure under the growth
operation `ω_j`, i.e. `∀x∀y {[ψ_j(x,y) ∧ Φ(x)] ⇒ Φ(y)}` (11); **(C)** downward
closure `∀x∀y<x {Φ(x) ⇒ Φ(y)}` (12).

This is a **definable cut closed under a fast-growing function instead of under
successor** — the `Ω_i`-style cut of the Pudlák / Wilkie–Paris tradition,
rendered inside `L*`. Compare `Willard2006a` Eq. (4) and `Willard2001` p. 5
(drift D32): the corpus now has four cut-like conditions.

**Theorem 3** (p. 16): for each consistent **regular** axiom basis α and each
fixed `J ≥ 1`, there is a formula `Φ(x)` that α can prove Locally-J-Closed.

> Due to a lack of page space, a formal proof of Theorem 3 will be **postponed
> until a longer version of this article**.

**The arXiv version is not that longer version** — it drops Theorem 3 entirely.
So the promised proof is in `Willard2021`, and this is direct textual support
for the priority raised on gap **G1** (previously inferred only from the 2021
title and date). Recorded as drift **D37**.

**Why Theorems 1 and 2 diverge** (p. 17) — the passage `Willard2020` has no
counterpart for:

> the needed diagonalization for producing Theorem 2's variations of the Second
> Incompleteness Effect become feasible **only when `IS_{Xtab}(β)`'s Linear-Sum
> Effect is applied to the intermediate results produced by its possible derived
> theorems** (which include the formalisms that are illustrated by lines (11)
> and (12)). On the other hand, no such similar types of nicely compressed
> constructed proofs are available under Theorem 1's `IS_{Tab-1}(β)` formalism
> (because all instances of the Law of Excluded Middle are excluded by it from
> becoming logical axioms).

with footnote 7 adding that **only "Locally 1-Closure" is needed** to show
`IS_{Xtab}(β)` cannot be self-justifying.

So the chain behind `Willard2020`'s ⊙ is: **LEM-as-logical-axiom → Linear-Sum
Effect → short proofs of the Locally-J-Closed conditions (11)/(12) → the
definable-cut diagonalization of Hájek–Pudlák pp. 172–174.** `Willard2020`
states the first and last links and omits the middle two. Obligation **O48**.

## 4. Provenance corroboration

- The Acknowledgment (p. 15) thanks **only Seth Chaiken**. `Willard2020`'s
  thanks **Seth Chaiken and James P. Torre, IV**.
- Theorem 1 here reads "the `IS_{Tab-1}(•)` and `IS_{Tab}(•)` **mapping are**
  consistency preserving" — the exact plural disagreement flagged at item 24 of
  `prose/willard2020draftreview.txt`, and corrected to "mappings are" in the
  arXiv version.

Both confirm the timeline recorded as D28: LFCS chapter (Jan 2020, uncorrected)
→ manuscript reviewed by jpt4 (May 2020) → arXiv v1 (Jun 2020, corrected, with
the added acknowledgment).

## 5. Errata

1. "mapping are" (Theorem 1) and "fails to be consistency-preserving mappings"
   (Theorem 2) — corrected in the arXiv version.
2. Appendix A's rules **5 and 6 are transposed** relative to `Willard2020`'s
   Appendix: here rule 5 is `∀`-elimination and rule 6 `∃`-elimination; there
   rule 5 is `∃` and rule 6 `∀`. Content identical.

## 6. Saturation record

| Pass | Date | Method | New items |
| --- | --- | --- | --- |
| 1 | 2026-08-26 | Item sweep across all 19 pp.; full read of Appendices A–B | 16 numbered items, 3 of them absent from `Willard2020` |
| 2 | 2026-08-26 | Visual control pass, p. 16 | 0 new; confirmed `ω₀(x) = x²`, `ω_{j+1}(x) = 2^{ω_j(2·Log₂(x+1))}`, `ω₁(x) ≥ x^x`, and Definition 4's three clauses |

§§1–4 were read only as deltas against `willard2020.md`. Saturation **open**.
