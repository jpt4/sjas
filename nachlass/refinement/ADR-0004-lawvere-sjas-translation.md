# ADR-0004 — Lawvere–Yanofsky Translation of SJAS / G2 Evasion

**Status.** **Complete 2026-09-02** — Main Theorem in
[`lawvere-sjas.md`](lawvere-sjas.md) §0 proved (G2 in-scheme; Aff missing
arrow for H4; necessary SJ conditions). Elevates the Lawvere hook that
ADR-0002 B3 recorded as *downgraded to a framing*.
**Branch discipline.** Logical units of this ADR land on `sjas-codification`
(or a dedicated ADR-0004 branch if concurrent work requires isolation).
**Depends on.** ADR-0001 (codified statement and registries); ADR-0002 R1–R3
(refined idea, margin, G2-ingredient classification).

## Context

The programme statement (`README.md`, `sjas.org`) names an explicit gap:
Lawvere / Yanofsky subsume Cantor, Russell, Tarski, Gödel's *First*, and
Halting, but **not** Gödel's Second. Filling that gap is what would connect
Willard's SJAS to LFPT and ask whether SJAS-style evasion generalises to the
other limitative fields.

`refined-sjas.md` §9 records the present state honestly:

> The Lawvere framing is a framing, not a theorem. … It does not give a
> categorical proof of any Willard result, and the claim that resource-bounded
> internal homs model the SJAS situation is a research direction, not a
> construction.

Yanofsky 2003 (arXiv:math/0305282, final section) independently lists G2 as an
instance of the scheme that is *not yet worked out*, noting that Kreisel's
model-theoretic diagonal proof of G2 "seems amenable to our scheme." So the
gap is recognised from both the SJAS side and the Lawvere–Yanofsky side.

The synthesis licenses exactly this departure:

> "This Refinement may deviate from Willard proper, e.g. by formulating the
> evasion of G2 in terms of **Lawvere's Fixed-Point Theorem**…"

## Decision

Treat the Lawvere–Yanofsky translation as a **named Refinement component R6**,
sibling to R1–R5. **Methodological order (binding):**

> First prove G2 using Lawvere / Yanofsky techniques — producing an explicit,
> numbered hypothesis list. Only then determine where Willard's constructions
> invalidate particular hypotheses of that limitative theorem.

This order is forced by the objective: evasion is meaningful only relative to a
stated proof. Using Willard's systems as external witnesses that "G2 needs more
than LFPT" (a separation *observation*) is allowed as motivation, but it does
not replace deriving G2 inside the scheme and then reading Willard as a
hypothesis-breaker.

Deliverables, in order:

1. **Lawvere–Yanofsky proof of G2.** A complete derivation of
   "consistent α cannot prove Con(α)" in which every appeal to LFPT /
   Yanofsky representability is marked, every further hypothesis is named
   (H0, H1, …), and the proof is self-contained enough that invalidating a
   single Hi blocks a named step. Primary route: Hilbert–Bernays–Löb with
   Lawvere supplying the diagonal lemma (matches Willard's own frame).
   Secondary routes to record, not substitute: Jeroslow + □-contraction
   (ALSJAS / Beklemishev–Shamkanov); Kreisel model-chain diagonal (the
   instance Yanofsky 2003 flagged as "amenable to our scheme").
2. **Hypothesis register.** A table mapping each Hi to the proof step it
   licenses, with a blank column for Willard's status (holds / fails / open).
3. **Willard invalidation.** For each affirmative Willard route, mark which
   Hi fails and cite the corpus locus; prove that the remaining hypotheses
   still give Group-3 / Lawvere representability.
4. **Self-justification conditions.** Necessary conditions as: all Hi needed
   for the Lawvere half, plus Con, plus consistency, plus failure of at least
   one post-Lawvere Hi — matching Willard's obligatory-breach theorem and the
   margin.
5. **Provenance discipline.** Same as ADR-0002 constraint 4.

Primary document: [`lawvere-sjas.md`](lawvere-sjas.md).
Working notes and failed attempts: `LOG.md` + optional `docs/log/` entries.

## Alternatives considered

- **Leave Lawvere as framing (status quo).** Rejected: the user's objective and
  the programme statement both require an actual translation with proofs.
- **Attack the fixed point (the "empty cell").** Deferred, not rejected: R3
  correctly notes that no known self-verifying theory removes the diagonal.
  This ADR proves *why* that cell is empty under CCC / cartesian-product
  hypotheses, and isolates what would be needed to occupy it (leave the
  Yanofsky product setting). Occupying it is out of scope for R6's first
  delivery.
- **Identify G2 with LFPT by force.** Rejected: that collapses the refinement's
  hard-won separation of (i) fixed point, (ii) HB conditions, (iii) semantic
  witness, (iv) □-contraction. The translation must *preserve* that
  classification inside categorical language, not erase it.

## Acceptance

- **L0** Methodological order respected: G2 proved in-scheme before Willard
  is used as a hypothesis-breaker (not merely as an external witness).
- **L1** Yanofsky product-form LFPT stated and proved in the document's own
  notation (citing Lawvere 1969 / Yanofsky 2003 / Yanofsky 2022 Thms 6.4–6.5).
- **L2** Full Lawvere–HB–Löb proof of G2 with every hypothesis Hi named and
  every step tagged by the Hi it consumes.
- **L3** Hypothesis register complete enough that "Willard evades G2" means
  "this named Hi fails at this corpus locus."
- **L4** Willard's routes mapped onto the register; Group-3 retained as
  Lawvere half intact.
- **L5** Necessary conditions for self-justification read off the register
  (which Hi may fail; which must hold), matching Willard's breach theorem and
  the margin.
- **L6** Explicit AAR after the first complete G2-in-scheme draft.

## Consequences

- ADR-0002 B3 is **reopened and upgraded**: Lawvere is no longer merely a
  framing once L1–L5 hold.
- R4 (Brown–Palsberg) and the computational-analogue programme gain a precise
  transfer target: an affordable subcategory of a Lawvere situation.
- The "empty cell" remains open as a separate research question; R6 explains
  why the known routes do not occupy it.
