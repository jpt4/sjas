# ADR-0002 — Refinement Charter

**Status.** R1 done and reviewed; R2 done; R3 partly done; R4–R5 pending.
**Reviewed and repaired 2026-09-02, twice** — a first pass against an external
review (22 corrections), then an independent adversarial review
([`REVIEW-2026-09-02.md`](REVIEW-2026-09-02.md)) which found ten more, including
two defects in the audit checks themselves. See
[`VERIFICATION.md`](VERIFICATION.md), this stage's registry, checked by
`../codification/audit.sh` (checks R-A–R-D).

## Context

`prose/sjas_synthesis.txt` specifies the Refinement in one sentence:

> "An immediate consequence of the Codified SJAS will be a 'Refined SJAS';
> taking inspiration from **Lev Beklemishev's claim of a simplified
> presentation of SJAS**, the essential *idea* of SJAS will be identified and
> explicated. This Refinement may deviate from Willard proper, e.g. by
> formulating the evasion of G2 in terms of **Lawvere's Fixed-Point Theorem**,
> or deriving the SJAS consistency preservation results from a **Rosser's Trick
> analogue**."

`ADR-0001` §"inherits" names what the Refinement receives: the codified
statement, the result matrix, the drift ledger, and four recorded hooks —
Beklemishev's simplification claim, the Lawvere-FPT and Rosser-analogue
formulations, and the Solovay private-communication thread.

## Decision

Refinement is a **stage**, sibling to Codification, living at
`nachlass/refinement/`. It differs from Codification in exactly one respect,
and the difference is licensed by the synthesis: **Codification is bound to
Willard's presentation; Refinement is bound only to the objective concept.**

Three constraints carry over unchanged, because they are what make the work
trustworthy rather than what make it faithful:

1. **Provenance for every claim about the literature.** A refinement may
   depart from Willard's formulation; it may not misreport it. Every statement
   here about what the corpus says is anchored in
   `../codification/registry/` and inherits that row's proof status.
2. **Proof status is never rounded up.** Where the refinement leans on a corpus
   result, it carries the result's status. §9 of `refined-sjas.md` exists for
   this.
3. **Departures are declared.** Where the refinement reformulates, it says so
   and says what is lost. Silent improvement is the failure mode this stage is
   most exposed to, since it is licensed to change things.

A fourth was added 2026-09-02, after a review found that **every Tier-1 defect
in this stage was of a kind the Codification's machinery exists to prevent**,
committed in the one stage that machinery did not reach:

4. **The Codification's verification discipline binds here.** Every quotation is
   image-verified — **extracted text is not a source**, only a way to find a
   page (standing instruction 2026-09-02; ADR-0001's visual-control rule as
   strengthened). Every claim about what the corpus says is checked against the
   extraction records before it is made; arithmetic claims are computed rather
   than read off a paper's informal gloss.
   [`VERIFICATION.md`](VERIFICATION.md) holds the register and
   `../codification/audit.sh` checks it.

Secondary literature is **in scope** here, having been out of scope for
Codification.

## Components

| Id | Component | Status |
| --- | --- | --- |
| **R1** | The essential idea: identify and explicate | **done 2026-09-01** — [`refined-sjas.md`](refined-sjas.md) |
| **R2** | Acquire and integrate the external refinements | **done 2026-09-01** — Beklemishev **unacquirable** (G7 closed: unpublished); **Pakhomov 2019 acquired and integrated** ([`R1-review.md`](R1-review.md)); **Beklemishev–Shamkanov 2016 acquired and assessed** ([`R2-beklemishev-shamkanov-assessment.md`](R2-beklemishev-shamkanov-assessment.md), G38 closed) — **R2 done** |
| **R3** | Make the criterion a definition, and prove the five dials are instances | **done 2026-09-02, partly — verdict at [`R3-the-margin.md`](R3-the-margin.md) §6**, written after re-reading `Willard2011` §§4–5 as page images. The definition holds and **is** `Willard2011` Def. 4.5, not a generalisation of it. **Three** dials (`ℜ`, `Herb−k`, `Z`) are now **proved** to move the margin, via the lemma that `♯` is independent of the deduction method. **Level(n)'s instance is retracted**; **Hybrid(H)'s** term is undetermined. The A-Stability ⟹ Tightness bridge completes but re-derives Willard's Thm 5.11. The cross-route unification is **refuted**, and Beklemishev–Shamkanov's system is **not self-verifying** |
| **R4** | The computational analogue: test the type-theoretic reading against Brown–Palsberg. **Carries obligation RO1**: affineness alone does not evade G2 — `□`-contraction can hold in affine PA (Beklemishev–Shamkanov §3) | pending |
| **R5** | Secondary-literature pass: Artemov, Pudlák, Visser, Adamowicz, Kolodziejczyk, Salehi | pending |
| **R6** | Lawvere–Yanofsky translation of SJAS / G2 evasion. **Order:** prove G2 in-scheme first (hypothesis register), then map Willard's breaks. Charter [`ADR-0004-lawvere-sjas-translation.md`](ADR-0004-lawvere-sjas-translation.md); result [`lawvere-sjas.md`](lawvere-sjas.md) | **done 2026-09-02** — Main Theorem proved: Thm 3.2 (G2), Aff missing-arrow Thm 5.5 (H4), SJ conditions Thms 6.2–6.5 |

R1 delivers the stage's stated deliverable. R3 is where it would become
mathematics rather than exposition.

## The blocked hook — resolved 2026-09-01

**G7 — there is no Beklemishev survey.** It was the *named* inspiration for
the stage, and it does not exist: Pakhomov 2019 footnote 3 records the
simplification as "still unpublished". G7 is closed `accepted` rather than left
pending, and B4 is replaced by B4′. What follows was written before that was
known.

`refined-sjas.md` §§1–5 is therefore an independent identification, and there is
nothing in the published record to reconcile it with. The risk the original
entry named — that an independent derivation might silently duplicate a
published one — does not arise.

The search for it was not wasted. It produced **Pakhomov 2019**, a published
external refinement that supplied a **second witness kind** and so motivated
generalising R1's criterion ([`R1-review.md`](R1-review.md) §2), and surfaced
**Beklemishev–Shamkanov 2016** (gap **G38**) as the next acquisition target.

*Corrected 2026-09-02:* an earlier version of this paragraph said Pakhomov
**falsified** R1's headline claim. He does not. `H_{<ω}` cannot prove successor
totality either — it evades Pudlák by Willard's own door — so it is not a
counterexample to the naming form, only a second instance of the general one.

## Acceptance

- **B1** The essential idea stated in a form a reader can check against the
  codified statement without reading Willard. *(R1 — met.)*
- **B2** Every departure from Willard's presentation declared, with what it
  costs. *(R1 §7 — met.)*
- **B3** The Lawvere and Rosser hooks discharged or explicitly downgraded.
  *(R1 §§2, 6 — met: Rosser is discharged as already-present-and-undeclared;
  Lawvere is downgraded to a framing, §9.)*
- **B4′** The identification compared against *published* external refinements. *(R2 — **met**. Pakhomov 2019 supplied a second witness kind and motivated the generalisation; Beklemishev–Shamkanov 2016 isolated a further prerequisite of G2's argument. The original B4, comparing Beklemishev's claim, is **unmeetable**: the work is unpublished, G7 closed.)*
- **B5** The criterion made a definition and its instances proved. *(R3 — **partly met, at three of five**. The definition exists and is Willard's own. **Three** dials are **proved** to move the margin — `ℜ`, `Herb−k`, `Z`, all of which vary only the deduction method, which `♯` provably ignores. **Level(n)'s instance is retracted** (it varies a sentence, and at the critical `β` Eq. (20) makes both terms move in lockstep). **Hybrid(H)** remains unplaced. D38 is answered for three, sharpened for one, reopened for one. The broader unification is **refuted**: self-verification is a classification, not a single condition, and breaking G2's argument is necessary but not sufficient for it.)*
- **B6** *(added 2026-09-02, restated after review)* Every mathematical quotation image-verified **and transcribed verbatim**, and every corpus claim status-carrying, per [`VERIFICATION.md`](VERIFICATION.md), with `audit.sh` green. *(**Met** for R1–R3 **as of the second pass**. The first pass asserted this while two quotations tagged "(image-verified)" were not verbatim — corrections #23 and #32. Note what green does **not** mean: `audit.sh` cannot check quotation fidelity. R-B tests only whether a `txt`-tagged register row carries hazard symbols; nothing diffs a quotation against its source. Verbatim transcription is a **human obligation** that the audit supports and does not discharge.)*

**The stage is not complete.** R1's deliverable is complete and has been reviewed
twice — against external literature, then against its own sources — and repaired.
B4′ and B6 are met; **B5 is partly met and its open half is answered in the
negative**: no single condition subsumes the routes, because they remove
different prerequisites of G2's argument, and removing one does not by itself
yield a self-verifying theory.

What remains open is smaller and sharper than before:

1. **Derive** each dial's transition from the inequality rather than reading it
   off Willard's results. Hybrid(H) is the tractable case *and* the unresolved
   one — its term is not identified (`R3-the-margin.md` §3.1), and the earlier
   `O(L)` criterion that placed it is refuted by arithmetic.
2. **The empty cell.** No known route attacks the fixed point itself, because
   Lawvere makes it free in any cartesian closed setting.
3. **Beklemishev–Shamkanov's cell has no theory in it.** Their own §6 names
   this: "we are still missing convincing examples of mathematical theories
   based on weak logics for which G2 would fail." With obligation **RO1** —
   `□`-contraction can hold in affine PA — the target is an arithmetic in which
   the *restricted* rule fails and `Con` is still provable.
