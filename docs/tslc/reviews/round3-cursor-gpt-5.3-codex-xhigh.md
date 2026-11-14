I’ve finished the cross-check and found one substantive inconsistency (h6 is added in §5.2/ADR but omitted in `RATIONALE.md`’s Phase A checklist), plus a couple of minor wording inconsistencies. I’ll now return a concise finding list and a direct verdict.
## Findings

| ID | Severity | Location | Argument | Fix |
|---|---|---|---|---|
| R3-01 | major | `RATIONALE.md` §9 (Phase A item 3) | Phase A still says lineage theorem uses `(h1)–(h5)`, but §5.2 adds critical runner invariant `(h6)` (the core round-2 concern). This leaves a plan-level inconsistency. | Change §9 item 3 to `(h1)–(h6)` and keep that consistent anywhere theorem prerequisites are listed. |
| R3-02 | minor | `RATIONALE.md` §7 capability table (“Safe”), §10 claim status (“Lineage theorem”) | Status text mentions projection lemma only, but §5.2 also marks P2 runner-correctness as required/open. This slightly understates open work. | Add P2 explicitly in both status rows. |
| R3-03 | minor | `ADR-0009-tslc.md` Context vs Decision | Context says candidate-behavior evaluation is not shown; later Decision says gate passed at toy size. This is internally inconsistent wording. | Reword Context to “not yet shown for agent-candidate projections/scale” (or equivalent). |

## R2-01..R2-06 resolution status

| Round-2 ID | Resolved in rev3? | Notes |
|---|---|---|
| R2-01 | yes | `RATIONALE.md` §5.3 now defines the target term explicitly (function case + agent-projection case). |
| R2-02 | yes (scoped as open proof) | §5.3 now explicitly marks gate soundness as a proof obligation with extractor-correctness + Reflect-case lemmas. |
| R2-03 | partly | `h6` is added in §5.2 and ADR, but §9 still lists only `(h1)–(h5)` (R3-01). |
| R2-04 | yes | Lineage proof now clearly marked pending on P1/P2, not presented as done. |
| R2-05 | mostly yes | Pre-registration structure and fail rule are now explicit in §9/ADR; concrete seeds/margins are intentionally deferred but clearly required before runs. |
| R2-06 | yes | b2/b3 safety preconditions and scoped “among mechanisms considered” language are now present. |

## Gate probe check (`probe-candidate-evaluation.*`)

- **Does it show §5.3’s claim?** Yes, at toy scale: output is `5` for the true candidate-on-3 derivation, and fallback `0` for wrong-input / other-function / unrelated-program derivations (`probe-candidate-evaluation.output.txt`).
- **Code-equality logic risk?** `codeeq` is structurally strict; I do not see an obvious false-accept path for a different term.
- **Term-extraction risk?** `termof` is a positional extractor (`left(right(left c))`), so correctness depends on matching the decoder’s actual root-term extraction. That exact correspondence is still an open proof obligation in `RATIONALE.md` §5.3. So: no concrete bug demonstrated, but this is still the key theoretical trust gap.

## What I checked and found correct

- `RATIONALE.md` now clearly scopes pending proofs (P1/P2) and avoids claiming full end-to-end completion.
- Gate target-term specification and gate-soundness obligations are explicitly stated.
- New probe evidence matches stated toy claim about candidate-output evaluation.
- Phase D now has an explicit falsification-oriented protocol structure (including tie=failure rule).

## Verdict

The account is now **mostly correct and honestly scoped**, and the plan is **sound as a plan** with pending proofs/experiment clearly marked. I would call it acceptable for proceeding, with the caveat that the three documentation inconsistencies above should be fixed, and formal closure still depends on P1, P2, and gate-soundness lemmas.
