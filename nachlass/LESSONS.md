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

- *A warm server's recheck outlives its client.* `bin/formal-check` killed by a
  timeout leaves the evaluation running server-side and the report is lost.
  Start it detached (`nohup bin/formal-check > test-runs/check-out.log &`) and
  wait for `failures` in the file; measure the server's CPU by the delta of
  `/proc/PID/stat` utime+stime (`ps`'s %CPU is a lifetime average).
- *Names that `cases` introduces are the constructor's field names* (`h`, `X`,
  `G`, …). A generated tactic list that does `(intro cd G h)` after
  `(cases X)` mixes them up for the constructors that have such fields; use
  names no field has (`cdx`, `Gx`, `hqx`).

- *A nondeterministic evaluator is made universal by determinism of the safe
  sub-relation, not of the evaluator.* Quantifying over all runs of a relation
  whose only free choice (a bnil tag) a safe run never observes: prove "every
  run of a source equal up to the free choice to a safe run is safe" by induction
  on the SAFE trace, inverting the arbitrary run at each node. Delete from the
  safe relation exactly the rules where the choice would be observed (here
  `apBnil`); re-proving the invariant for arbitrary runs would need a new
  logical relation.
- *Inversion of an indexed family by a function.* When `cases` on the family is
  unavailable, define `Inv index := (recursion on the index)`, with one clause
  per rule that can conclude it, generated from the rule table; one induction
  proves `Fam index → Inv index`; `constructor`/`exT` see through the folded
  definition. Generate, don't write.
- *`cases`/`induction` on a nested inductive (RV with `List RV`) build ill-typed
  terms in this kernel*; apply the nested recursor (`RV.rec`, `RV.rec_1`) by
  hand, with Prop motives written inline (a kdef'd motive is not unfolded when
  elaborating the minor premises).
- *Equality of values up to a forgotten component is a function (`flV`), not a
  relation*: injectivity and inversion are one-line `congrArg`s through
  projections defined by the recursor; no `injection` or `noConfusion` is
  needed.
- *`exT` is for `Type` witnesses*; for a derivation (Prop) witness use an
  `Exists.rec` at `Prop` (`exP`).
- *`subst h` with `h : a = b` eliminates `a`* (the left side) — orient the
  equation so the variable to remove is on the left; keep a second hypothesis
  named for the survivor only.
- *A hypothesis the rule does not record cannot be recovered from a derivation:*
  the universal evalE theorem needed `D` to be a base type at reflect, which the
  typing gives but `OkE` omitted; the cheapest fix was the premise in the trace
  rule (the existential proof had the fact in scope), not an invariant on
  values.

