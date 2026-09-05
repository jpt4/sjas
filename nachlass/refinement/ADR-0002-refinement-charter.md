# ADR-0002 — Refinement Charter

**Status.** R1 done and reviewed; R2 done; R3 partly done; **R6 complete and completion-reviewed 2026-09-04**; R4–R5 pending.
**Reviewed and repaired on 2026-09-02 and 2026-09-04** — an external review (22
corrections), an independent adversarial review
([`REVIEW-2026-09-02.md`](REVIEW-2026-09-02.md), ten more including two defects
in the audit checks), then eight further adversarial rounds which found, among
others, that the margin's sufficiency rests on a `sketch`, that Pudlák is not
held at all, and three separate checks that reported green because their own
matching was broken. Corrections #1–#69 are tabulated in
[`VERIFICATION.md`](VERIFICATION.md). See
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
| **R4** | The computational analogue: test the type-theoretic reading against Brown–Palsberg. **Carries obligation RO1**: affineness alone does not evade G2 — `□`-contraction can hold in affine PA (Beklemishev–Shamkanov §3). **And, from R6 (`ADR-0004` Consequences, recorded here 2026-09-04): a precise transfer target** — a coded modal proof doctrine in which the boxed diagonal `copy_A : □A → □A ⊗ □A` and the uniform proof-composition map are exhibited *separately*, so a candidate type theory can be tested against each independently rather than against "self-reference" as a whole | pending |
| **R5** | Secondary-literature pass: Artemov, Pudlák, Visser, Adamowicz, Kolodziejczyk, Salehi | pending |
| **R6** | Categorical comparison of AU incompleteness, Beklemishev-Shamkanov, and Willard SJAS. **Order:** prove G2 in-scheme first (hypothesis register), then map the distinct breaks. Charter [`ADR-0004-lawvere-sjas-translation.md`](ADR-0004-lawvere-sjas-translation.md); result [`lawvere-sjas.md`](lawvere-sjas.md) | **corrected and completion-reviewed 2026-09-04** -- Theorem 1.1 isolates the selected Gödel fixed point, HBL maps, and boxed contraction; §§2-4 give the AU, B-S, and Willard presentations. The invalid `Aff` construction is withdrawn; Willard's G2 and Group-3 fixed points are separated; B-S external consistency is recorded separately from its missing internal consistency point |

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
  *(Rosser — **met** at R1 §6, discharged as already-present-and-undeclared.
  Lawvere — **reopened**. R1 §9 downgraded it to a framing, and this entry
  recorded that as the disposition; [`ADR-0004`](ADR-0004-lawvere-sjas-translation.md)
  then reopened and **upgraded it to a comparison theorem**, delivered as **R6**
  ([`lawvere-sjas.md`](lawvere-sjas.md)) — explicitly **not** a claim that
  Willard's systems are arithmetic universes. ADR-0004's Consequences section
  said so from the start; this entry did not, and the two charters disagreed
  until 2026-09-04.)*
- **B4′** The identification compared against *published* external refinements. *(R2 — **met**. Pakhomov 2019 supplied a second witness kind and motivated the generalisation; Beklemishev–Shamkanov 2016 isolated a further prerequisite of G2's argument. The original B4, comparing Beklemishev's claim, is **unmeetable**: the work is unpublished, G7 closed.)*
- **B5** The criterion made a definition and its instances proved. *(R3 — **partly met, at three of five**. The definition exists and is Willard's own. **Three** dials are **proved** to move the margin — `ℜ`, `Herb−k`, `Z`, all of which vary only the deduction method, which `♯` provably ignores. **Level(n)'s instance is retracted** (it varies a sentence, and at the critical `β` Eq. (20) makes both terms move in lockstep). **Hybrid(H)** remains unplaced. D38 is answered for three, sharpened for one, reopened for one. The broader unification is **refuted**: self-verification is a classification, not a single condition, and breaking G2's argument is necessary but not sufficient for it.)*
- **B6** *(added 2026-09-02; restated twice)* Every mathematical quotation image-verified **and transcribed verbatim**, and every corpus claim status-carrying, per [`VERIFICATION.md`](VERIFICATION.md), with `../codification/audit.sh` green. *(**Met as of the tenth pass, and this criterion has now been declared met and then falsified twice** — first when two quotations tagged "image-verified" proved non-verbatim (corrections #23, #32), then when a sweep found the status-carrying half broken in ten places, including `Willard2001` Thm A.1 and `Willard2011` Lemma 4.6 (#62–#66). Neither half is machine-checkable: R-B tests register hygiene, not fidelity, and R-E is informational because no positional heuristic captures "at the point of use". **Treat any future assertion that B6 is met as a claim requiring a fresh sweep, not as a standing fact.** The first pass asserted this while two quotations tagged "(image-verified)" were not verbatim — corrections #23 and #32. Note what green does **not** mean: `../codification/audit.sh` cannot check quotation fidelity. R-B tests only whether a `txt`-tagged register row carries hazard symbols; nothing diffs a quotation against its source. Verbatim transcription is a **human obligation** that the audit supports and does not discharge.)*

**The stage is not complete.** R1's deliverable is complete and has been reviewed
repeatedly — against external literature, then against its own sources — and
repaired at each pass; the corrections are numbered #1–#95 in
[`VERIFICATION.md`](VERIFICATION.md), which is the record to consult rather than
a count restated here. B4′ is met. **B5 is partly met at three of
five**, and its open half is answered in the negative: no single condition
subsumes the routes, because they remove different prerequisites of G2's
argument, and removing one does not by itself yield a self-verifying theory.

What remains open:

1. **The margin's sufficiency has no `full` proof in the corpus.** `Willard2011`
   Lemma 4.6 — Tightness implies self-justification — is `sketch`, and §5's
   rigorous engine (Thm 5.9) consumes EA-stability instead. Until that gap is
   closed, the definition R3 supplies is a definition whose *point* rests on a
   sketch (`R3-the-margin.md` §§2.2, 6).
2. **Derive** each dial's transition from the inequality rather than reading it
   off Willard's results. Hybrid(H) is the tractable case *and* the unresolved
   one — its term is not identified (`R3-the-margin.md` §3.1), and the earlier
   `O(L)` criterion that placed it is refuted by arithmetic.
3. **R6's non-claims are the shape of its open half.** The categorical
   comparison ([`lawvere-sjas.md`](lawvere-sjas.md) §6) explicitly does not show
   `IS(A)` is an arithmetic universe, does not make `A ↦ □A` an endofunctor in
   Willard's doctrine, and does not claim a blocked derivation suffices for
   self-justification. Its AU source's §6 Löb derivation is sketch-level.
4. **R5 has not been started, and one gap in it is load-bearing.** Artemov,
   Visser and Salehi are held; Adamowicz and Kołodziejczyk are not; and
   **Pudlák is not held at all** (gap **G39**) although the Refinement invokes
   his theorem throughout, entirely through Willard's and Pakhomov's
   descriptions.
5. **The empty cell.** No known route attacks the fixed point itself, because
   Lawvere makes selected diagonals free in any cartesian closed setting — and
   R6 sharpens what "the fixed point" means: the G2 diagonal `G ↔ ¬□G` and the
   self-consistency diagonal `H ↔ Con(B+H,D)` are distinct, and Willard adopts
   only the second.
6. **Beklemishev–Shamkanov's cell has no theory in it.** Their own §6 names
   this: "we are still missing convincing examples of mathematical theories
   based on weak logics for which G2 would fail." With obligation **RO1** —
   `□`-contraction can hold in affine PA — the target is an arithmetic in which
   the *restricted* rule fails and `Con` is still provable.
