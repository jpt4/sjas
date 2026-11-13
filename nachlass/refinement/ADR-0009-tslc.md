# ADR-0009 — TSLC: self-modifying agents in λᶜᵉʳᵗ

**Status.** Proposed 2026-11-13. Awaiting the user's decision. Revised the same
day: the agent layer needs no change to the calculus (section "The five items").

**Branch.** Recorded on `f6-p5`. Each phase gets its own branch when it starts
(`adr-0009-a`, …).

**Depends on.** [R4](R4-metatheory.md) §§1–5; [ADR-0006](ADR-0006-formalization.md)
(Lemma 3.6, CheckSpec and Theorem 4′ in Ansatz); [ADR-0008](ADR-0008-extraction.md)
(the extracted `Check`, which the runner should use once it lands).

## Context

λᶜᵉʳᵗ₀ derives its own consistency over the certificates it holds. It has no way
to replace itself: a run is one program at a fixed budget, and choosing the next
program is the runner's act, which the theory does not cover.

The analysis of 2026-11-12 and 2026-11-13 asked what a self-modifying agent needs,
and checked the answer on a toy in Ansatz
([note](../../docs/log/2026-11-13-tslc-walkthrough.md),
[toy](../../docs/log/2026-11-13-tslc-toy.clj)):

- **W1.** Every action in every round is safe, whatever successors it adopts.
- **W2.** It decides each adoption with its own checker.
- **W3.** Inside the language, it can rely on what a successor it adopts will do.
- **W4.** It derives its own consistency (H₀, H₁) and is consistent.
- **W5.** It has W3 unconditionally, for successors given as code of any size.

The toy establishes:
- W1, as a theorem about the language proved outside it;
- W2;
- W3 in two forms: unconditionally for successors the agent holds as certificates,
  and from the next round on for successors given as code;
- W4.

W5 is impossible: trust in free code without the delay derives ⊥ by Löb's
argument.

## The five items

The toy left five things to the real calculus. Reassessed, none needs a change
to λᶜᵉʳᵗ₀ for W1, W2 and W4. A probe,
[`2026-11-13-agent-probe.clj`](../../docs/log/2026-11-13-agent-probe.clj),
supports this.

1. **Tokens** (H₁'s two certificates share one budget). This is already
   kernel-checked as T2. No change below touches it.
2. **The checker.** No new rule is needed. The agent type is
   `Π(d :ω Syn). Π(self :ω Syn). Π(i :ω I). O × Σ(c :ω Syn). Σ(b :ω Bool). T(if b (chk c d) tt)`.
   - It uses only Π, Σ, `T`, `chk` and dependent Bool elimination.
   - The agent checks its candidate with `chk` at run time, when the codes are
     closed and `chk` computes.
   - The runner ties the knot outside the calculus: it passes d = ⌜Agent⌝ and the
     agent's own code.
   - The probe type-checks two such agents (derivations of about 1,800 nodes) and
     runs a four-round lineage. Each round, the runner decodes the accepted code,
     runs that term and re-checks the successor. The outputs are 0, 2, 2, 2, and
     the lineage stops when a candidate fails the check.
3. **`next`, application under `later`, guarded recursion.** These are needed only
   for static reasoning, inside the language, about the unbounded future. W1 does
   not need them:
   - the type carries each round's guarantee;
   - a goal that spans rounds becomes an invariant on state passed to the
     successor, with a decreasing measure if it must be reached;
   - the lineage theorem covers all rounds.

   The agent type already contains a run-time "later": a code with its check.
   These rules are deferred to Phase C.
4. **Agents with inputs.** Safe self-improvement needs no held adoption. Every
   candidate that checks at the agent type is safe to run, so an agent can try one
   for a round, measure it, and hand back. Handing back works if the parent's code
   travels in the state.
   - For certified use of a candidate within a round, `reflect` at base types
     works per input today. Build the derivation code of the candidate applied to
     the input, projected to its base-type output; pay for it from a supply;
     `inspect` it; `reflect` it.
   - Extending `reflect` to first-order types would save that per-input
     certificate. That is Phase B, optional.
5. **The runner's count of handed-over certificates.** Certificates are never
   handed across rounds; codes are. A successor that wants to hold one pays for it
   from its own budget. Nothing needs proving.

**One constraint found:**
- A candidate's code written into program text as a literal is recorded again at
  every node of the program's derivation (the full-judgment encoding).
- Certifying such a program exhausted a 1.5 GB heap.
- So candidates travel as data: as input or state.

## Decision

Build the agent layer on λᶜᵉʳᵗ₀ as it stands, in one phase. Keep two extensions
as optional, and run the experiments on the agent layer. Every phase follows the
project's rules:
- red tests before green;
- statements kernel-checked in Ansatz in the exact form R4 states them, with an
  axiom audit;
- results relative to the metatheory;
- an after-action report.

### Phase A — The agent layer (W1, W2, W4)

1. **The agent type**, as a library definition with no new rule:
   - the type above;
   - a variant that passes state, `Π(s :ω S)…`, which also returns the next state.
2. **The runner**, `lc/run-agent`. Each round it:
   - decodes the current code and runs its term on (⌜Agent⌝, the code, the
     input, the state) with the tokens the derivation declares;
   - when the agent's verdict is true, re-checks the successor with the trusted
     `Check`;
   - records the round and continues with the successor's code, or stops.

   It hands only codes and data across rounds. Until ADR-0008 lands, the current
   checker is the test oracle.
3. **The lineage theorem, in Ansatz.** For every closed derivation at the agent
   type, every input sequence and every k:
   - round k's output lies in O's semantic set;
   - its successor, if the verdict is true, is again a closed derivation at the
     agent type.

   The proof is an induction on k over results already proved:
   - Lemma 3.6 at `Check`: the decoded agent's denotation lies in its type;
   - CheckSpec: an accepted code decodes to a derivation, with m < ‖c‖;
   - Theorem 4′'s agreement at base types: the output, the successor code and the
     verdict are base data.
4. **Tests and examples:**
   - the probe's cases, as red-then-green tests;
   - an agent that tries a candidate for a round and hands back;
   - an agent whose state carries an invariant across rounds.
5. **Documents:** an R4 section on agents, and a tutorial section.

**Success:** the lineage theorem is kernel-checked, and the runner's tests are
green. **Failure:** the type or the theorem needs a change to the calculus.
Record what is needed, and reopen the five items.

### Phase B (optional) — `reflect` at first-order types

Extend `reflect` from base data types to functions between base data types:
- the result keeps the certificate's footprint;
- the decoded program runs on its input at budget n − (‖v‖ − m).

This saves the per-input certificate of held adoption. The work is a Lemma 3.4′,
the Reflect case, the Ansatz proofs and the evaluator. Phase D's measurements
decide whether it is needed.

### Phase C (optional, research) — the static time guard

The rules are ▷, `next`, application under ▷, guarded recursion and `become`. The
work:
- the checker's F7 cases;
- consistency, T2 and P5 by reading ▷A as the one-element type;
- the meaning theorem, by a reading over rounds with Kripke-style functions;
- termination with `next` and `fix` suspended.

The toy already establishes the result at its level. Build this only if an
experiment needs static reasoning about the unbounded future.

### Phase D — Experiments, after Phase A

1. **Self-improvement by trial.** Candidate successors are generated, tried for a
   round and kept or handed back. Record what is adopted and its cost. By the
   lineage theorem there are no unsafe actions.
2. **Held commitments.** A lineage keeps the code of a commitment. Each round
   holds a certificate of it, and H₁ excludes holding a certificate of its
   negation. Measure what this costs and what it excludes.
3. **Report:** a note in `docs/log` and an entry in `LOG.md`.

### Later, as separate ADRs

- A checker the calculus can reason about, for adopting a faster checker by an
  internal proof.
- Bets: named assumptions about stronger checkers, with blame.

## Order and resources

- **Phase A** can start now on its own worktree. It changes nothing that p2, s3 or
  x8 work on.
- **Phase D** follows A. B and C are taken up only if D shows a need.
- **Lanes:** one Codex lane for the Ansatz work, within the memory rule (at most
  one warm server and one suite). Claude handles the runner, tests, design and
  review. Starting a lane is the user's decision.

## Open decisions for the user

1. Accept this plan.
2. Start Phase A now, or after x8 or p2.

## Success and failure for the ADR as a whole

**Success:**
- agents run indefinitely with renewed budgets and checked hand-overs;
- the lineage theorem is kernel-checked for the real calculus;
- the experiment report exists.

**Failure** of a phase is recorded with what the agent loses. **No phase aims at
W5,** which is impossible.

## After-action report

Pending.
