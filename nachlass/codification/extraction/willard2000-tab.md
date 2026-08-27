# Extraction: Willard2000-TAB

> Tier B extraction (component C10), recorded as a **variant witness** against
> [`willard2002a.md`](willard2002a.md). Read in full rather than as deltas —
> the `Willard2020-LFCS` case showed that a conference version can carry
> material its journal expansion drops. **Anchoring**: PDF page *n* = printed
> page *n + 414* (PDF 1 = LNAI 415).

## 1. Identity and witness

| Field | Value |
| --- | --- |
| Corpus key | `Willard2000-TAB` |
| Title | The Semantic Tableaux Version of the Second Incompleteness Theorem Extends Almost to Robinson's Arithmetic Q |
| Venue | TABLEAUX 2000, Springer LNAI **1847**, pp. 415–430 |
| Witness | `nachlass/papers/willard2000_tableaux_robinson_q_author_tab2000.pdf` (16 pp.) |
| Funding | NSF Grant CCR 99-02726 |
| Relation | The conference form of `Willard2002a` (JSL 67, 2002, pp. 465–496), whose §1 calls this "presented initially as a 16-page conference abstract" |

## 2. Item correspondence with `Willard2002a`

| `Willard2000-TAB` | `Willard2002a` |
| --- | --- |
| Definition 1 (`Log(x,k)`, `SemPrf^K_α`) | Definition 2.1 |
| Theorem 1 (tableaux closure via cut-elimination) | Theorem 2.2 |
| Theorem 2 (the three-condition inconsistency) | Theorem 2.3 |
| Lemmas 1, 2, 3 | Lemmas 3.1, 3.2, 3.3 |
| **Theorem 3** (Q+V consistent) | Theorem 3.4 |
| **Theorem 4** (no finite extension proves its tableaux consistency) | Theorem 3.5 |
| Lemmas 4, 5, 6 | Lemmas 4.1, 4.2, 4.4 |
| Definition 2 (closed subtree), Lemma 7, Definition 3 (canonical binary), Lemmas 8, 9, 10 | Definition 4.5, Lemma 4.6, Definition 4.3, Lemmas 4.7, 4.8, 4.9 |
| *(absent)* | §5 (Conventional Deciphering Property, `V₆`, Theorem 5.4, Remark 5.6), §6 (hyper-inclusion, Theorem 6.4 answering Paris–Wilkie, Theorem 6.6), Appendix |

## 3. Deltas that matter

Nothing here is absent from `Willard2002a` — unlike the LFCS case, the journal
version is a strict superset in content. But four constants and one structural
detail differ, and a codified statement drawing on either must not blend them:

1. **`V₂` has four clauses here, six in `Willard2002a`.** Lemma 2 gives
   `A₁`–`A₄`; the journal version's Lemma 3.2 gives `A₁`–`A₅` and adds `A₆`, the
   associativity/commutativity/distributivity axiom for Addition and
   Multiplication. So the threshold sentence `V` is genuinely weaker here.
2. **Lemma 5's constant is `Log(z, 2^{3,000})`**; `Willard2002a` Lemma 4.2 has
   `Log(z, 2^{1,000})`.
3. **Lemma 6's bound is `O(s·n^m)`**; `Willard2002a` Lemma 4.4 has `O(s·n^{2m})`.
4. **Lemma 10 is a Proof Sketch**; `Willard2002a` Lemma 4.9 supplies a "More
   Detailed Justification" with the `f(z)`/`M`/`z ≤ M` case split.
5. **Adamowicz is cited only for `IΣ₀ + Ω₂`** (ref. [1]); by 2002 Willard also
   cites Adamowicz–Zbierski and an unpublished Adamowicz for `IΣ₀ + Ω₁`. The
   two motivating questions on p. 2 are correspondingly posed against `Ω₂` alone.
6. The infinite-cardinality generalisation is noted but "we will have
   insufficient space to prove this generalization here" — it becomes
   `Willard2002a` §5's Theorem 5.2.

**An unpublished intermediate exists.** Lemma 1's proof refers to "**the
unabridged version of this paper (which the author can mail to any interested
readers)**". That is neither this chapter nor, on the evidence, `Willard2002a`
— which supplies its own Appendix instead. Recorded as gap **G30**.

**`V₄`'s comment is already here** (p. 7): "a redundant axiom can
super-exponentially shorten the length of some cut-free proofs (a fact that we
will use when we prove Lemma 9)". So drift **D30**'s observation is stated in
2000, two years before `Willard2002a`'s CLARIFICATION — and, per the G29 pass,
the underlying construction is in TR 93-10.

**Comment on the deliberate lopsidedness** (p. 8) is also already here, in the
same words as `Willard2002a`'s Comment 3.6: the choice of `V` makes Theorem 4's
proof "astonishingly short" at the cost of Theorem 3's.

`C ≅ 12` (Lemma 7) and the canonical binary representation (Definition 3) match
`Willard2002a` exactly.

## 4. Saturation record

| Pass | Date | Method | New items |
| --- | --- | --- | --- |
| 1 | 2026-08-27 | Full text read of pp. 1–16; uncapped, case-insensitive item sweep | 10 Lemmas, 4 Theorems, 3 Definitions — all corresponding to `Willard2002a` items |

| 2 | 2026-08-27 | **Visual control pass**, pp. 7, 9, 10 | 0 new items; all four divergences confirmed on the page |

Coverage **complete** (pp. 1–16). Pass 1 recorded the divergent constants
without an image, on the reasoning that `Willard2002a`'s counterparts had been
image-verified at C9. That reasoning was wrong — the whole point of those rows
is that the constants **differ**, so the C9 verification could not cover them.
The pass has been run: `Log(z, 2^{3,000})` and its "more than 3,000 bits"
justification, `O(s·n^m)`, "all four clauses of Equation (12)'s `V₂`", `C ≅ 12`
and Definition 3's `O(Log N)` are all confirmed as printed.
