# ADR-0010 — Tiler: self-modifying agents on TSLC's time guard

**Status.** Proposed 2026-11-20. Supersedes ADR-0009's Phase A agent layer and its Phase D.
The prototype (item 1) was built under the user's license of 2026-11-20 to design and
build "something that could be immediately coded up and played with".

**Branch.** `tiler`.

**Rationale.** [`docs/tslc/TILER.md`](../../docs/tslc/TILER.md). This ADR records only the
decision.

## Context

- ADR-0009 placed the trusted runner outside the agent. Yudkowsky and Herreshoff (2013,
  §5.3) name that move, a split decision criterion, as a way to dodge the Löbian obstacle
  by giving up naturalism. Under it no self-trust is needed, which is why the TSLC
  rationale kept finding self-justification unnecessary.
- In a naturalized agent, with nothing outside its own code to stand guard, self-trust
  is unavoidable: approving a self-modifying successor relies on the successor's future
  checks. The trust is sound only at a strictly smaller measure. For successors, the
  measure that never runs out is time, which is TSLC's `▷`/`become` (YH13 §4.2's `T_κ`,
  without κ). The size guard (`reflect`) trusts only what is already held, and would
  shorten the lineage at every generation.
- Tiler v1 shows this. Under `:never` the lineage never starts. Under `:same` the
  agent's own search installs the Löbian wirehead. Under `:later` the agent solves its
  task with no write to the reward or the off switch, and 400 random self-modifying
  programs make none either.

## Decision

1. **Tiler v1, the sandbox** (done). `code/tiler/`: interpreter, checker with the
   self-trust settings, world, agent, proposer, experiment; 32 tests.
2. **Soundness in Ansatz.** Model Tiler's language and checker in Ansatz. Prove that
   accepted code never writes an environment slot at any tick, by induction on ticks
   and on fuel. Kernel-check the `:same` counterexample. Relate the install rule to the
   toy's `become` (`sBecomeSpec`).
3. **Tiler v2, proofs for semantic safety.** The checker accepts code together with a
   certificate of a safety property, rather than by syntax alone. λᶜᵉʳᵗ₀'s `chk` and its
   certificates are the format, and `become` is install. Repeat the three-setting
   experiment at the level of proofs.
4. **Naturalism.** Add executors in other slots. Test that building an agent elsewhere
   and rewriting oneself pass one rule, and that no special case is needed.
5. **Checker replacement.** Allow installing a stricter checker. Show that a looser one
   cannot be approved.
6. **Dropped from ADR-0009.** Phase D's comparison of `reflect` with a step-limited
   evaluator. Also the outside-runner agent type as the vehicle for self-justification
   research. The size guard stays, as the `:fuel` rule and as a result about held
   certificates.

**Success:**
- Item 2 kernel-checked.
- Item 3 reproduces the three outcomes with proofs in place of syntax.
- Item 4 needs no new rule.

**Failure:**
- The Ansatz proof needs a hypothesis Tiler's code does not meet. Record it and fix the
  checker.
- Item 3's proof checker cannot express the install rule without making `:same`
  derivable. Record what separates them.

## Order and resources

- Items 2 and 4 are independent and can run in parallel.
- Item 3 follows item 2.
- Item 5 is small.
- Claude does items 1, 4 and 5 and drafts item 2. A Codex lane for the Ansatz proof is
  the user's decision.

## Open decisions for the user

1. Accept this ADR in place of ADR-0009's Phase A and D.
2. Whether to start a Codex lane for item 2.

## After-action report

Pending.
