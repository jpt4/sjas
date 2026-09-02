# ADR-0002 — Refinement Charter

**Status.** R1 done and reviewed; R2 done; R3 partly done; R4–R5 pending.
**Reviewed and repaired 2026-09-02** — see [`VERIFICATION.md`](VERIFICATION.md),
which is now this stage's registry and is checked by
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

4. **The Codification's verification discipline binds here.** Every quotation
   containing mathematics is image-verified; every claim about what the corpus
   says is checked against the extraction records before it is made; arithmetic
   claims are computed rather than read off a paper's informal gloss.
   [`VERIFICATION.md`](VERIFICATION.md) holds the register and
   `../codification/audit.sh` checks it.

Secondary literature is **in scope** here, having been out of scope for
Codification.

## Components

| Id | Component | Status |
| --- | --- | --- |
| **R1** | The essential idea: identify and explicate | **done 2026-09-01** — [`refined-sjas.md`](refined-sjas.md) |
| **R2** | Acquire and integrate the external refinements | **done 2026-09-01** — Beklemishev **unacquirable** (G7 closed: unpublished); **Pakhomov 2019 acquired and integrated** ([`R1-review.md`](R1-review.md)); **Beklemishev–Shamkanov 2016 acquired and assessed** ([`R2-beklemishev-shamkanov-assessment.md`](R2-beklemishev-shamkanov-assessment.md), G38 closed) — **R2 done** |
| **R3** | Make the criterion a definition, and prove the five dials are instances | **done 2026-09-02, partly** — [`R3-the-margin.md`](R3-the-margin.md). Definition given (the **margin** `Log(q_β) − ♯(β)`, generalising `Willard2011` Def. 4.5). **Four** dials shown to lower `Log(q_β)`; the fifth (Hybrid(H)) moves the margin by a route not settled here. Instances **argued, not proved**. The cross-route unification is **refuted** — and the roster corrected: Beklemishev–Shamkanov's system is **not self-verifying** |
| **R4** | The computational analogue: test the type-theoretic reading against Brown–Palsberg. **Carries obligation RO1**: affineness alone does not evade G2 — `□`-contraction can hold in affine PA (Beklemishev–Shamkanov §3) | pending |
| **R5** | Secondary-literature pass: Artemov, Pudlák, Visser, Adamowicz, Kolodziejczyk, Salehi | pending |

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
- **B5** The criterion made a definition and its instances proved. *(R3 — **partly met**. The definition exists. **Four** of Willard's five dials are shown to move its cost term, answering drift **D38** for those four and sharpening the question for the fifth. The instances are argued from cited results, not derived from the inequality — and most of those results are `sketch` or `stated-only`. The broader unification R2 had made the target is **refuted**: self-verification is a classification, not a single condition, and breaking G2's argument is necessary but not sufficient for it.)*
- **B6** *(added 2026-09-02)* Every mathematical quotation image-verified and every corpus claim status-carrying, per [`VERIFICATION.md`](VERIFICATION.md), with `audit.sh` green. *(**Met** for R1–R3; binds R4–R5.)*

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
