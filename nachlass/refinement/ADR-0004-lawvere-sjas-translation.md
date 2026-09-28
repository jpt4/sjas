# ADR-0004 — Lawvere–Yanofsky Translation of SJAS / G2 Evasion

**Status.** **Complete, corrected 2026-09-04; one scope gap recorded
2026-09-04** — Pakhomov's `H_{<ω}`, one of the two demonstrated routes to a
self-verifying theory per R1–R3, is absent from the comparison and its absence
was undeclared. Recorded in [`lawvere-sjas.md`](lawvere-sjas.md) §6.
**Complete otherwise** — the comparison theorem and
three categorical presentations are in [`lawvere-sjas.md`](lawvere-sjas.md).
The 2026-09-02 `Aff` formulation was withdrawn: a class expressly not closed
under composition is not a category. The replacement uses a genuine syntactic
category/proof doctrine for Willard, a symmetric monoidal Lindenbaum category
for Beklemishev–Shamkanov, and the source's actual AU construction.
**Branch discipline.** Logical units of this ADR land on `sjas-codification`
(or a dedicated ADR-0004 branch if concurrent work requires isolation).
**Depends on.** ADR-0001 (codified statement and registries); ADR-0002 R1–R3
(refined idea, margin, G2-ingredient classification).

## Context

The programme statement (`../../README.md`, `../../sjas.org`) names an explicit gap:
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

1. **G2 comparison theorem.** State the selected Gödel/Löb fixed point, HBL,
   structural, and propositional hypotheses separately, then derive the
   incompatibility of an internal consistency point with external consistency.
2. **AU baseline.** Read van Dijk–Oldenziel's actual `R`, `Gamma`, internal-Hom
   evaluation, classifying-AU implication, and fixed-point constructions into
   that register. Do not silently assume cartesian closure.
3. **B–S presentation.** Present their APS as a thin category and their
   multiset consequence relation as a symmetric monoidal closed proof category.
   Record that all three Löb conditions hold while boxed contraction fails.
4. **Willard presentation.** Use the cartesian syntactic category and its
   predicate hyperdoctrine. For Type-A `IS(A)`, identify the missing *uniform*
   HBL-(2) entailment. Keep separate (a) the selected Gödel fixed point used by
   the G2 proof under Theorem A.1's strength hypothesis and (b) the selected,
   fixed-numeral Group-3 self-consistency diagonal. Do not assign the same
   breach to every Willard route.
5. **Self-justification distinction.** Require both an internal consistency
   point and external consistency. G2-evasion alone is only a negative result.
6. **Provenance discipline.** Same as ADR-0002 constraint 4; all load-bearing
   formulas and quotations are checked against rendered source pages.

Primary document: [`lawvere-sjas.md`](lawvere-sjas.md).
Working notes and failed attempts: `../LOG.md` + optional `docs/log/` entries.

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
- **L1** The common package explicitly separates fixed points, HBL (1)–(3),
  boxed contraction, the internal consistency point, and external consistency.
- **L2** The AU instance cites the exact §6 arrows and explains where cartesian
  structure supplies contraction.
- **L3** The B–S instance corrects the premise that an HBL condition fails:
  L1–L3 hold; the boxed diagonal does not.
- **L4** The Willard Type-A instance names HBL (2) at its corpus locus and
  distinguishes fixed proof-code instances from an internal uniform arrow.
- **L5** The selected Gödel fixed point and selected Group-3 self-consistency
  fixed point are distinguished from one another and from the universal
  fixed-point property of the AU; all-route generalisation is explicitly open.
- **L6** The final table records that B–S's `S` is externally consistent but
  only blocks formalized G2, whereas Willard additionally supplies the internal
  consistency point and an external consistency theorem for the theory that
  contains it.

## Consequences

- ADR-0002 B3 is **reopened and upgraded to a comparison theorem**, not to a
  claim that Willard's systems are AUs.
- R4 (Brown–Palsberg) and the computational-analogue programme gain a precise
  transfer target: a coded modal proof doctrine with the boxed diagonal and
  uniform proof-composition map shown separately.
- The "empty cell" remains open as a separate research question; R6 explains
  why the known routes do not occupy it.

## AAR (2026-09-04) — three-way AU / B–S / Willard cell

The first draft correctly separated HBL from boxed contraction, but represented
Willard's internal weakness by `Aff`, a “wide class” explicitly allowed to fail
composition. That made its missing-arrow claim noncategorical and concealed the
important pointwise/uniform distinction. The corrected document uses the
cartesian syntactic category of the weak arithmetic and places proof witnesses
in its predicate hyperdoctrine. HBL (2) is now a missing uniform entailment in a
genuine proposition category. It also retracts the claim that Group-3 supplies
universal Lawvere representability: the AU proves fixed points for every
endomorphism in Lemma 6.12, whereas Willard uses selected fixed-numeral
instances. The completion review further separates the standard Gödel fixed
point displayed in Willard 2001, Theorem A.1 footnote 16, from the distinct
Group-3 self-consistency fixed point, and records that B-S's `S` is externally
consistent even though it has no internal consistency point.
