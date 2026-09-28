# Lessons

*Durable lessons from the SJAS work. The chronological record is
[`LOG.md`](LOG.md); each lesson points to the entry it came from.*

## A correction needs the same scrutiny as the claim it corrects

On 2026-09-25, while writing the R4 metatheory, I corrected the draft in seven
places. On 2026-09-26, two independent reviews found that two of those
corrections were themselves wrong:
- `H` does follow from `H₁` inside the calculus;
- `R` does have a definable destructor.

The draft's original sentences had been closer to the truth. A correction
feels like progress and invites less suspicion than a new claim; it deserves
more.

*Apply:* send corrections through the same independent review as new results,
and record the corrections as claims with their own status. From
`LOG.md`, 2026-09-26, review round 1.

## A semantic bound is not an operational one until erasure is accounted for

The model of the R4 metatheory bounds certificates by the tokens supplied. The
non-erasing evaluator nonetheless builds, in an erased position, a certificate
of any size from a single token (review R4-04). Resource claims about running
programs hold only for an evaluator that never runs erased positions.

*Apply:* when a type system's guarantee is stated for runtime values, say
which evaluator it holds for, and test it there. From `LOG.md`, 2026-09-26.

## A test suite that is green on its first run needs a coverage check

The `Check` tests of the λᶜᵉʳᵗ₀ implementation passed on their first run.
Before trusting them, two things were measured: which rules the corpus
exercised (all 39), and whether the nested-`Check` test really went through a
δ-step (it did, through a 74-step chain).

*Apply:* when tests pass immediately, confirm what they exercise before
counting them as evidence. From `LOG.md`, 2026-09-26.

## A proof assistant's automation is not its kernel

The Ansatz kernel accepted every theorem of the λᶜᵉʳᵗ₀ kernel. Three things in
the surrounding tooling did not work:
- `grind` looped or gave up;
- the generated equation lemmas were unusable in one case;
- one tactic sequence produced a proof term the kernel rejected.

The working proofs used only `change`, checked definitionally by the kernel,
and `omega`.

*Apply:* when automation fails, fall back to steps whose correctness the kernel
itself decides, and record which ones worked. From `LOG.md`, 2026-09-26.
