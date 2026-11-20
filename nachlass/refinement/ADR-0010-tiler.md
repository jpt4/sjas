# ADR-0010 — Tiler, and what to build for self-modifying agents

**Status.** Proposed 2026-11-20, revision 2 (after review round 5).
- Supersedes ADR-0009's Phase A agent layer and its Phase D.
- The prototype (item 1) was built under the user's 2026-11-20 license to build "something that
  could be immediately coded up and played with".

**Branch.** `tiler`.

**Rationale.** [`docs/tslc/TILER.md`](../../docs/tslc/TILER.md).

## Context

- **ADR-0009's agent layer.** It is the split decision criterion of Yudkowsky and Herreshoff
  (2013, §5.3): a separate rule approves a successor that satisfies the same check. It dodges
  the Löbian obstacle rather than meeting it. YH13 allow the dodge for software that does not
  rebuild its own hardware.
- **Tiler v1.**
  - It is the same dodge, executed by the agent's own code, with no runner outside the agent.
  - The agent improves itself and never writes its reward, although its own trials favour doing
    so.
  - No self-justification is involved: the install rule is sound by an invariant the designer
    proves.
  - A fuel-guarded check-then-run stub gives the same lineage as the time-guarded install.
  - A learner that never modifies itself does best on the task.
- **Where self-justification would be needed.** Only when an agent must prove, from its own
  axioms, that its successor's verifier is sound. There, Löb's theorem forces a weaker successor
  or a guard:
  - the time guard, `T_κ` or `▷`, with κ moved into the semantics;
  - possibly size with income.

  Nothing shown so far suggests this adds capability.

## Decision

1. **Tiler v1** (done): `code/tiler/`, 41 tests, the experiment of TILER.md §5.
2. **Self-modifying agents use the split-criterion design**, with a fixed checker and a
   designer-proved invariant. No self-justification is built for agents.
3. **Optional research, the user's decision: Tiler v2.**
   - The checker becomes agent code.
   - Installs require the agent's own proof that the successor's verifier is sound.
   - Compare the guards: none, which gives the telomere; time; size with income; and the naive
     rule, which gives Löb.
   - Include a capability test against baselines without self-trust.
   - Prediction: no guard adds capability over the split-criterion baseline. A pass would need
     explaining.
4. **Optional: the soundness of Tiler v1 in Ansatz.** It is a routine invariant proof that
   checks the machine; it is not progress on the obstacle.

**Success (item 3):**
- the obstacle is live: the naive rule is exploitable and no guard yields a telomere;
- each guard's lineage and capability are measured against the baselines.

**Failure (item 3):** the setting cannot be made live without stipulating the outcome. Record why.

## Open decisions for the user

1. Accept this ADR in place of ADR-0009's Phases A and D.
2. Whether to pursue item 3.

## After-action report

Pending.
