# C4 Fidelity Gate: Willard2005 extraction vs. the known-good re-derivations

Charter ADR-0001 criterion **A3**. The pilot extraction
([`willard2005.md`](willard2005.md)) is checked against the two prior
partial re-derivations of the same paper, which were produced independently of
this workstream:

- **R1** — `docs/design/affine-tree-sjas.md` §2–§3 (the affine-tree design
  document's re-derivation of the 2005 preservation engine), with its summary
  in `docs/adr/ADR-0001-affine-tree-sjas.md` ("Decisive Finding").
- **R2** — the 2026-07-27 Willard deductive-apparatus survey, Proflog commit
  `a7af9f7`, `docs/interdev/2026-07-27-willard-deductive-apparatus-survey.md`.

Purpose of the gate: divergence between the extraction and a known-good source
either exposes an extraction error (fix the record) or an error in the prior
work (record it, and fix the template if the template let it through).

## Verdict

**Gate passed.** The extraction agrees with R1 and R2 on every load-bearing
definition, theorem statement, and proof structure. Three divergences were
found, all in R1, all in the **same** place — the relation between the coding
density and the compactification exponent θ — and none of them changes R1's
design conclusions. Two precision items and one mathematical side condition
are recorded for the codified statement.

No template change is required. One extraction hazard was discovered and is
recorded for corpus-wide use (§Hazard).

## Agreements (spot-checked against the primary source)

| Item | R1's rendering | Primary source | Verdict |
| --- | --- | --- | --- |
| Definition 4 Normed(a,b) | "Every axiom is Π\*1 with App∀(a) or Σ\*1 with App∃(b); the language has the eight U-grounding function symbols and *absolutely no others*" | Def. 4, p. 19, both clauses verbatim in substance | ✅ exact |
| Definition 5 θ-compactification | Both parts, with the side condition `p ≤ ⌈a/b⌉+1` and θ < 1/3 | Def. 5 + Eq. (18), p. 19 | ✅ exact |
| Theorem 1 statement | "If D is θ-compactified (θ < 1/3), then `IS_D(·)` is consistency-preserving" | Thm. 1, p. 19 | ✅ exact |
| Theorem 1 proof recital | minimal pair (j₁,j₂), m = max; Groups 0–2 true; Group-3 falsified only above m−1 ⇒ Normed(m−1,1); envelopes `(m−1)/m^θ` and `m^θ`; θ < 1/3 ⇒ contradiction | pp. 19–20, step for step | ✅ exact, including the `Normed(m−1, 1)` step |
| App∀/App∃ semantics | envelopes restrict **unbounded** quantifiers only | Eqs. (16)–(17) + p. 19 note | ✅ exact |
| U-Length / U-Depth / U-Height | "per sentence / accumulated / max over leaves" | p. 22, items 1–3 | ✅ exact |
| Valuation bound | `Val(u) ≤ b·2^Δ(s)`, justified by "each U-grounding function symbol at most doubles a value" | Eq. (19); Case (c) p. 24 ("the Double operation is the fastest available growth function") | ✅ exact |
| Lemma 1 | Positive Branch; hypothesis U-Height < log₂a − log₂b; greedy `Probe(a,b,T)`; **nine-case** induction | Lemma 1, p. 23; nine subcases (a)–(i), pp. 24–25 | ✅ exact, case count included |
| Lemma 2 | Non-Closure | Lemma 2, p. 25 | ✅ exact |
| Remark 4 destruction mode | multiplication as a ninth function symbol "overwhelms Definition 5's constraints"; no analogue for any deduction method once multiplication is total | Remark 4, p. 21, verbatim | ✅ exact |
| `IS_D(β)` ↔ `ISD(A)` naming | R1 states the correspondence | §3, p. 8 | ✅ |
| Cut-free sequent calculus is affirmative for this profile | R2: "Willard says in the 2005 JSL paper that the `IS_D(.)` transformation is consistency-preserving for it after the required compactification analysis… Remark 1 states this application but says the detailed case analysis is omitted for space" | Remark 1, p. 20 | ✅ exact, **including the proof-status caveat** |

R2 is accurate on every 2005 claim checked. Its ledger entry (line 514)
correctly reads Remark 1 as a direct affirmative application whose case
analysis is omitted.

## Divergences (all in R1, all one root cause)

### F1 — The density constant bounds θ from **below**, not above

R1's ADR states: *"the coding charges ≥ 5 bits/symbol… The ratio yields the
compactification exponent **θ ≤ 1/5** < 1/3 that Theorem 1 needs."* R1's design
document §3 states: *"θ = 1/c < 1/3 follows"* (with `c ≥ 4`), and §3's balance
paragraph: *"their ratio is **θ ≤ 1/c** < 1/3."*

The primary source has a different logical shape. The two constraints on θ are:

1. **From Definition 5** (Theorem 1's hypothesis): `θ < 1/3`.
2. **From the density**, via Lemma 1's hypothesis: in Theorem 2's proof the
   enlarged system `Z` is Normed(`p^θ·b`, `b`), so Lemma 1 requires
   `U-Height(p) < Log₂(p^θ·b) − Log₂ b = θ·Log₂ p` (footnote 5, p. 27).
   Equation (20) supplies `U-Height(p) < (1/5)·Log₂ p`. This suffices
   **iff `θ ≥ 1/5`**.

So the admissible window is `1/5 ≤ θ < 1/3`, and Willard picks **θ = 1/4**
(Theorem 2, p. 26; Theorem 4, p. 28) — a value strictly inside it, matching
neither `1/c` nor `≤ 1/5`. A *larger* θ is a *weaker* compactification claim
and is therefore easier to satisfy; density buys a *floor*, and Definition 5
imposes the *ceiling*.

Generalizing: for coding density `d` (i.e. `U-Height(p) < (1/d)·Log₂ p`), the
window is `1/d ≤ θ < 1/3`, which is **non-empty exactly when `d > 3`**.

**Impact on R1: none of its conclusions falls.** R1 chose density 5, which
gives the window `[1/5, 1/3)`; R1's headline claim that the affine-tree
calculus is "(1/5)-compactified" is a legitimate choice of θ within its own
window (indeed the tightest one), and R1's remark that *"density 3 is the
θ = 1/3 boundary itself"* is **correct** under the corrected reading — it is
exactly the `d > 3` condition. What is wrong is only the stated derivation
("the ratio yields θ", "θ = 1/c", "θ ≤ 1/5"), which collapses a window to a
point and inverts an inequality. R1's own §8 states the relation correctly in
passing — *"his tableau analysis lands at 1/4 from a coding of ≥ 4–5
bits/symbol"* — so R1 is internally inconsistent between §3/§4 and §8.

### F2 — Three distinct constants must not be conflated: 5, 6, and 1/4

R1's dictionary row reads *"≥ 5 bits per proof symbol (B-adic) ↦ ≥ 5 nodes per
quoted node (density-5 literals)"*. The 2005 source has three separate
quantities:

| Quantity | Value in Willard 2005 | Where |
| --- | --- | --- |
| Bits per **language symbol** in the Gödel encoding | **6** (a "byte" is six bits; base-64; 24 symbols coded 32–55) | Appendix A, p. 39 |
| Bound on **U-Height** relative to `Log₂ p` | **1/5** | Eq. (20), p. 26 |
| Compactification exponent **θ** | **1/4** | Thms. 2, 4 |

Equation (20) is not "bits per symbol": U-Height counts only *U-Grounding
function symbols accumulated along one branch*, whereas the encoding charges
six bits for every symbol of every sentence in the whole tree. Eq. (20) is
therefore an a-fortiori consequence of the encoding, and a deliberately
conservative one. The "≥ 5J bits for J function symbols ⇒ Gödel number ≥ 32^J"
formulation that R1 quotes is **Willard 2011's** Definition D.1 part (iv),
where the 2005 inequality is promoted to a definitional clause admitting "any
natural method". R1 cites the 2011 formulation in a 2005 context.

**Impact on R1: none substantive** — the affine-tree design charges 5 nodes per
quoted node and takes the 2011 condition as its model, which is the right
model to take. The precision matters for the codified statement, which must
present the 2005 and 2011 forms as the same condition under two presentations
(cross-paper link flagged in the extraction record §6; to be confirmed at C6).

### F3 — Proof-status caveat dropped for the non-tableau apparatuses

R1 §10 writes that *"all of Willard's affirmative apparatuses (Tab, Res,
cut-free sequents) are normal-form-only calculi"* and treats cut-free sequents
as an established affirmative case for this profile. In the 2005 source, the
only witness for cut-free sequent calculus, Herbrand deduction, Tab−Q\*₁−List,
and the Q\*₁-restricted Hilbert variant is **Remark 1 (p. 20), which is
explicitly stated-only** — Willard names the method (show Definition 5, then
apply Theorem 1) but says "we will not have the page space to do so."

R2 records this correctly; R1 does not carry the caveat. This is a
proof-status omission, not a factual error, and it strengthens gap **G8** (the
corpus states but does not print several apparatus-specific compactification
analyses). The same caveat attaches to Remark 6 (tangibility hybridization,
"details… very lengthy… therefore not done here") and to all three variants of
Remark 8.

## Observation for the codified statement (not a divergence)

**The small-`m` side condition in Theorem 1.** Both Willard (p. 20, "since
θ < 1/3 implies g > h") and R1 ("θ < 1/3 ⇒ m^(2θ) < m−1") state the final
inequality as immediate. It is not quite unconditional: `g > h` unfolds to
`m − 1 > m^{2θ}`, which for θ = 1/4 requires `m > (3+√5)/2 ≈ 2.618`, i.e.
`m ≥ 3`. The condition is harmless — `m` is the Gödel number of a proof and is
astronomically large — but it is a genuine side condition, and the codified
statement should discharge it explicitly rather than inherit the gloss from
both sources.

## Hazard discovered (corpus-wide)

`pdftotext` renders JSL small-caps item headings with a space after the
initial letter: `D EFINITION 4.`, `T HEOREM 1.`, `L EMMA 2.`, `R EMARK 4.`,
`C OROLLARY 1.` A naive `grep -E '^(Definition|Theorem|Lemma)'` therefore
matches **only prose cross-references and misses every actual heading** — it
returned zero true headings on this paper while appearing to succeed. Every
JSL-typeset witness in the corpus is affected (`Willard2001`, `Willard2002a`,
`Willard2006b`), and other publishers' small-caps conventions must be probed
per-venue before an inventory sweep is trusted. The reproducible sweep and
this warning are recorded in the extraction record §8.

## Actions taken

| Action | Where |
| --- | --- |
| Record F1/F2/F3 as drift-ledger entries against R1 | `../concordance/drift-ledger.md` D6 |
| Raise the Remark-1 stated-only finding against gap G8 | `../registry/gaps.md` G8 |
| Record the small-`m` side condition as an obligation for the codified Theorem 1 | drift-ledger D6, note |
| Record the extraction hazard | extraction record §8; this file |

The affine-tree ADR and design document are **not edited** by this workstream —
they belong to a different branch and workstream. The corrections are recorded
here and in the drift ledger so that whoever next revises them, or composes
the codified statement, inherits the exact finding.
