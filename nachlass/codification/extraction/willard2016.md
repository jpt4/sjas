# Extraction: Willard2016

> Tier B extraction (component C10). **Version note**: the witness is
> arXiv:1612.08071 **v5, dated 12 Oct 2017**, not the 2016 v1 the corpus key
> suggests. Its own comment: "All the theorems and propositons are the same in
> this Version 5 as in Version 4. The difference is that the writing style is
> now significantly more polished." `nachlass/papers/willard2017.pdf` and
> `lit/willard2017thetafunction.pdf` are the same document.

## 1. Identity and witness

| Field | Value |
| --- | --- |
| Corpus key | `Willard2016` |
| Title | On How the Introducing of a New θ Function Symbol Into Arithmetic's Formalism Is Germane to Devising Axiom Systems that Can Appreciate Fragments of Their Own Hilbert Consistency |
| Venue | arXiv:1612.08071v5 [math.LO], 12 October 2017 (never journal-published) |
| Witness | `nachlass/papers/willard2016_theta_function_symbol_arxiv_1612.08071.pdf` (34 pp.) |
| Funding | NSF Grant CCR 0956495 |
| MSC | 03B52; 03F25; 03F45; 03H13 |

## 2. Role in corpus — the one result the corpus leaves conditional

The abstract says it plainly: "Only one intermediate result, which most readers
will intuit should be true, **does remain formally unproven**." That result is
**Conjecture 6.6**, and **Theorem 6.7 — the paper's consistency-preservation
result — is conditional on it**. This is the only affirmative result in the
corpus resting on an unproved lemma, and `Willard2020` §8 reports it as
near-certain ("as we are almost certain it is"). Gap **G21** is closed by
recording it here; the claim itself belongs in the frontier chapter, flagged.

## 3. The θ primitive and `IQFS`

**θ** is proposed as a primitive that "almost achieves the combined efficiency
of the addition, multiplication and successor growth operations", and it is
**the only growth-permitting function in the language `L_Q`**.

- **Definition 4.4**: `θ^j(x)` is the `j`-fold iterate; Definition 4.4's `E_j`
  terms use θ to define `2^j`.
- **Proposition 4.3**: an integer `n` is encoded as a grounded term `T_n` using
  `O{[Log(n)]³}` logical symbols — against `O(n²)` for the weaker primitive `ζ`
  of Eqs. (10)–(11), and against the `O{Log(n)}` one would ideally want. §7
  reports that the encodings can be improved to `O{Log(n)}`.
- **Definition 4.7 / Example 4.8**: a ground term is **Observable** under the
  stated condition; e.g. `Max(θ(C₁), C₂)`.
- **Definition 5.1** gives the `Δ^Q₀`, `Π^Q_n`, `Σ^Q_n` classes — a **ninth**
  class notation across the corpus. Critically, **`Π^Q₁` and `Σ^Q₁` sentences
  forbid θ's appearance** except inside the `E_j` terms.
- **Definition 5.7**: **`IQFS`** = "**Introspective Q-Function System**", a
  four-group system in the corpus's usual shape; `IQFS(PA+)` verifies isomorphic
  counterparts of all Peano Arithmetic's `Π₁` theorems and corroborates a
  fragmentary definition of its own **Hilbert** consistency.
- **`UNION(β)`** again denotes Groups Zero, 1 and 2 (as in `Willard2001`
  Appendix B and `Willard2006a` Lemma 1).

## 4. Conjecture 6.6 and Theorem 6.7

> **Conjecture 6.6.** Suppose γ is an axiom system that includes (A) all IQFS's
> Group-zero and Group-1 axioms, and (B) a set of additional `Π^Q₁` axioms all
> true in the Standard Model except for one special `Π^Q₁` sentence Ψ that
> constitutes a **Size-`K` Breaking Point** (`K > 2`). Suppose `P` is a proof
> from γ of `0 = 1` under `d_ER`. Then
>
> ```
> Log₂K / Log₂P  <  1/6                                   (28)
> ```

Willard: "We are **essentially 100 % confident** that Conjecture 6.6 is true.
Indeed, Line (28)'s estimate … is actually an excessively conservative
over-estimate. An approximate intuitive justification … is provided in the
attached appendix. (It actually falls **only one tiny iota short of being a
formal proof**.)"

**The `1/6` is the corpus's recurring density constant** — `Willard2001` Eq. (26)
carries `2^{⌊β_i/6⌋−2}` and its Appendix B defines a byte as six bits. Drift
**D25** gains a fifth instance.

> **Theorem 6.7.** Conjecture 6.6 implies that IQFS's framework satisfies
> Definition 5.2's Consistency Preservation property — if β is consistent with
> IQFS's Group-0 and Group-1 axioms then `IQFS(β)` is consistent.

**Proof** (the corpus's standard architecture, with one new link): take `P`
minimal proving `0=1` from `IQFS(β)`; Lemma 6.3 encodes the Group-3 axiom as a
`Π^Q₁` sentence `∀x ψ(x)` (29); since `P` exists, that sentence is false, so it
owns a Size-`K` Breaking Point; Conjecture 6.6 gives `K < P`; contradiction with
`P`'s minimality. Remark 6.8 notes this is the one place the corpus's
consistency-preservation proofs route through an unproved intermediate.

**Appendix A.1's justification.** Because θ is the only growth-permitting
primitive and `Π^Q₁`/`Σ^Q₁` forbid it outside the `E_j` terms, "a proof can
construct integers `n > 2` only by applying the **Up-Walking axioms**, from
Lines (14)–(17), in the directly canonical and cumbersome manner" — so `P` can
verify `n ≥ 2^d` only after isolating `d+1` distinct terms for `d+1` distinct
powers of 2. With `M_d` the finite model of `{x < 2^d}` and `d = ⌊Log₂K⌋`, every
axiom of `S` holds in `M_d`, the Breaking Point Ψ becomes false in `M_{d+1}`,
and the rest survive in `M_{d+1}, M_{d+2}, …`. Hence `P` establishes `0=1` only
after constructing more than `1 + ⌊Log₂K⌋` distinct terms.

**Where the iota sits.** The argument's load-bearing steps are hedged — "These
facts **seem to obviously imply**", "a proof **appears to be able to** verify" —
i.e. it establishes that the *canonical* route to large integers is cumbersome,
not that no cleverer proof structure exists. That is exactly the gap between a
lower bound on one strategy and a lower bound on all of them.

## 5. Platonic Stability, and Willard's own summary judgement

**Definition 8.1** introduces **Platonic Stability**; **Corollary 8.2** applies
it, showing that "adequate forms of Platonic Stability are available to allow an
introspective thinker to simultaneously: 1. **presume its own consistency as a
built-in assumption**, and 2. **rest assured this assumption will not spin its
`IQFS(β)` formalism into a cycle of inconsistency**". Willard says this
"partially reinforces **Gödel's philosophy of Mathematical Platonism**".

**Remark 8.3 (Snapshot Perspective)** is his 25-year retrospective, and it
concedes the same point as `Willard2011` Remark 6.16b (obligation **O24**):

> Summarizing our last 25 years of research into one short paragraph, it is
> certainly true that any proof that relies upon Example 3.5's "I am consistent"
> axiom is, in some respects, a **quite skinny form of proof**, that one is
> almost first tempted to ignore.

And §9 states the programme's limit in his own voice — quotable for the codified
statement's honesty, alongside obligations O24 and O35:

> All our published articles about self-justifying arithmetics have emphasized
> that evasions of the Second Incompleteness Effect rested on using arithmetics
> that were **weaker than traditional arithmetics** in, at least, some respects.
> (The Second Incompleteness Theorem's significance in refuting the original
> objectives of Hilbert's Consistency Program is thus, simply, **undeniable**.)

Obligation **O65**.

## 6. Numbered-item inventory (selected)

| Label | p. | Digest | Proof |
| --- | --- | --- | --- |
| Example 3.1, Definition 3.2, Example 3.3 | 6 | Apparatus separation; "Hilbert-style" deduction; the `++` invariant of Pudlák, Solovay, Nelson and Wilkie-Paris | cited |
| Definition 3.4, Example 3.5 | 8 | Self-justification for `(α, d)`; the `⊕` "I am consistent" axiom | n/a |
| Definition 3.6 | 9 | Formal functions relative to a fixed γ | n/a |
| Definition 4.1, 4.2 | 10–12 | Function symbols; the languages `L_Q` and `L_Q^…` | n/a |
| **Proposition 4.3** | 12 | An integer `n` needs `O{[Log n]³}` symbols as a grounded term under θ, against `O(n²)` under ζ | full |
| Definition 4.4, 4.5, 4.7; Remark 4.6; Example 4.8 | 12–13 | `θ^j`; terms; **Observable** ground terms | n/a |
| Remark 4.9 | 14 | The paper's goals | n/a |
| Definition 5.1 | 16 | `Δ^Q₀`, `Π^Q_n`, `Σ^Q_n` — θ forbidden outside `E_j` terms | n/a |
| Definition 5.2, Example 5.3 | 18 | The mapping `I(•)` and **Consistency Preservation** | n/a |
| Definition 5.4, Example 5.5, Definition 5.6 | 18–19 | `L_Q`; the `d_ER` deductive method | n/a |
| **Definition 5.7** | 20 | **`IQFS`** — "Introspective Q-Function System" | n/a |
| Definition 6.1, Lemma 6.2 | 22 | `Ax_α`; `ProofCheck_α(s)` is `Δ^Q…` | full |
| Lemma 6.3 | 22 | `IQFS(β)`'s Group-3 axiom has a `Π^Q₁` encoding | full |
| Remark 6.4 | 23 | Groups 1 and 2 are not given `Π…` form here | n/a |
| Definition 6.5 | 23 | **Size-`K` Breaking Point** | n/a |
| **Conjecture 6.6** | 24 | `Log₂K / Log₂P < 1/6` — **unproven** | stated-only |
| **Theorem 6.7** | 24 | Conjecture 6.6 ⟹ `IQFS(•)` is consistency-preserving | full, **conditional on 6.6** |
| Remark 6.8 | 25 | This is the only consistency-preservation proof in the corpus routed through an unproved intermediate | n/a |
| Proposition 7.1 | 25 | A **Dag**-analog of §4's formalism, in place of the Tree-oriented ground terms | full |
| Remarks 7.2–7.5 | 27 | `IQFS*`; the infinite axiom set; amendments; θ's philosophical interest | n/a |
| Definition 8.1, Corollary 8.2, Remark 8.3 | 29 | **Platonic Stability**; its application; the 25-year retrospective | full / n/a |

## 7. Saturation record

| Pass | Date | Method | New items |
| --- | --- | --- | --- |
| 1 | 2026-08-27 | Uncapped, case-insensitive item sweep across all 34 pp.; full read of §§6, 8–9 and the Appendix; statement-level read of §§1–5, 7 | ~30 numbered items |

Coverage **partial** — §§1–5 and §7 read at statement level. Gap **G31** extended.
No visual pass yet; the `1/6` of Eq. (28) and Proposition 4.3's `O{[Log n]³}`
were read from a clean LaTeX text layer but should be image-checked at C13.
