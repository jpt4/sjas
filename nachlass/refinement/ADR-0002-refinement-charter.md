# ADR-0002 — Refinement Charter

**Status.** Opened 2026-09-01. R1 done; R2–R5 pending.

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

Secondary literature is **in scope** here, having been out of scope for
Codification.

## Components

| Id | Component | Status |
| --- | --- | --- |
| **R1** | The essential idea: identify and explicate | **done 2026-09-01** — [`refined-sjas.md`](refined-sjas.md) |
| **R2** | Acquire and integrate Beklemishev (gap **G7**) | **blocked** — not held; see below |
| **R3** | Make the §5 criterion a definition, and prove the five dials are instances | pending |
| **R4** | The computational analogue: test the type-theoretic reading against Brown–Palsberg | pending |
| **R5** | Secondary-literature pass: Artemov, Pudlák, Visser, Adamowicz, Kolodziejczyk, Salehi | pending |

R1 delivers the stage's stated deliverable. R3 is where it would become
mathematics rather than exposition.

## The blocked hook

**G7 — Beklemishev's survey is not held.** It is the *named* inspiration for
the stage. `refined-sjas.md` §1–§5 is an independent identification of the
essential idea, arrived at from the codified material; whether it agrees with
Beklemishev's simplification is undetermined and undeterminable without the
paper.

This is recorded rather than papered over because the risk is specific: an
independent derivation that happens to coincide with a published one should be
credited to it, and one that diverges should be reconciled with it. Neither can
be done now. **Acquiring G7 is the first action of any continuation**, and R1
should be re-read against it before the Refinement is called complete.

## Acceptance

- **B1** The essential idea stated in a form a reader can check against the
  codified statement without reading Willard. *(R1 — met.)*
- **B2** Every departure from Willard's presentation declared, with what it
  costs. *(R1 §7 — met.)*
- **B3** The Lawvere and Rosser hooks discharged or explicitly downgraded.
  *(R1 §§2, 6 — met: Rosser is discharged as already-present-and-undeclared;
  Lawvere is downgraded to a framing, §9.)*
- **B4** Beklemishev's claim compared. *(R2 — **not met**, blocked on G7.)*
- **B5** The criterion made a definition and its instances proved. *(R3 — not
  met.)*

**The stage is not complete.** R1's deliverable is complete; B4 and B5 are
outstanding, one blocked and one open. Recording this rather than declaring
completion is the same discipline the C16 review applied to its own image
queue.
