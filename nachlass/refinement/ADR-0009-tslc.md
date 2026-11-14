# ADR-0009 — TSLC: self-modifying agents in λᶜᵉʳᵗ

**Status.** Proposed 2026-11-13. Awaiting the user's decision. Revision 4
(2026-11-14), after four rounds of adversarial review.

**Branch.** Recorded on `f6-p5`. Each phase gets its own branch when it starts.

**Depends on.** [R4](R4-metatheory.md); [ADR-0006](ADR-0006-formalization.md) (Lemma
3.6, CheckSpec and Theorems 4 and 4′ in Ansatz); [ADR-0008](ADR-0008-extraction.md)
(extraction).

**Rationale.** [`docs/tslc/RATIONALE.md`](../../docs/tslc/RATIONALE.md) gives the full
reasoning, the evidence and the review record. This ADR records only the decision.

## Context

The rationale establishes four points:
- **Safety.** Safe self-modification is ordinary proof-carrying code: typed hand-over
  checked by a fixed checker, with safety as an external theorem. It runs on
  λᶜᵉʳᵗ₀ unchanged.
- **Self-justification.** λᶜᵉʳᵗ₀'s self-justification is the postulates `H` and `H₁`,
  shown safe by budget descent. They never run.
- **The executable principle.** It is `reflect`: bounded local reflection at base
  types, whose ⊥-instance is `H`. It lets a running program extend itself with code
  received at run time, with completion guaranteed. Its token cost is known in
  advance; its running time is not.
- **The competitor.** A step-limited evaluator written in λᶜᵉʳᵗ₀ evaluates received
  code during a run without self-justification. It needs no certificate, works on
  any input and bounds time, but can run out of steps. `reflect`'s edge over it is
  completeness and some lesser conveniences (round 4).
- **Hence.** Self-justification is not needed for any agent capability shown so far.
  Its value is in showing where sound self-trust stops, which is the research aim.
- **What remains open.** It is not yet shown that this helps an agent improve itself
  beyond what trial or wrapper adoption already gives. An agent can evaluate a small
  function candidate's own behaviour this way (the gate probe); doing so for agent
  candidates, at scale, is not yet shown.

## Decision

### Phase A — the agent layer on λᶜᵉʳᵗ₀ as it stands

1. **Engineering gates:**
   - candidates travel as data, never as literals in program text;
   - the round's token supply is an argument `r :₁ R`, and the round runs in
     Θ_{m+k′}, with the supply's k′ tokens distinct from the m that the agent's
     derivation declares;
   - the runner evaluates the decoded derivation directly, with the literals in its
     environment, and does not re-type each round;
   - a budget policy caps or charges the successor's declared budget m′;
   - the runner logs every certificate a round builds;
   - memory ceilings are stated, and runs are niced and capped.
2. **The agent type**, as a library definition:
   `Π(d :ω Syn). Π(self :ω Syn). Π(i :ω I). Π(r :₁ R). O ⊗ Σ(c :ω Syn). Σ(b :ω Bool). T(if b then chk c d else tt)`,
   with I base, and `O := Σ(a :₁ Nat). T(safe a)` for a closed `safe` that contains no
   `reflect`. Add a variant that passes state.
3. **The runner** `lc/run-agent`, with the gates above. Until ADR-0008 lands, it uses
   the current checker as the test oracle.
4. **The lineage theorem in Ansatz**, under hypotheses (h1)–(h6) of the rationale
   §5.2. It needs two lemmas:
   - P1, the projection lemma: Theorem 4′'s agreement for the three base-type
     projections of an agent's output;
   - P2, runner correctness: the runner implements the formal decoder, evaluator
     and `Check`. This is met by extraction.
5. **The gate probe.** An agent evaluates a candidate's own behaviour on a fixed test
   input:
   - it checks a proposer-supplied derivation at the base output type;
   - it verifies, by code equality written in λᶜᵉʳᵗ₀, that the derivation's root term
     is the candidate's term applied to the input;
   - then it pays for the derivation and runs it with `reflect`.

   Its target term is defined in the rationale §5.3. Gate soundness (extractor
   correctness plus the Reflect case) is a proof obligation of this phase.
   This is go/no-go for Phase D's regime (t). Alongside it, write the step-limited
   evaluator in λᶜᵉʳᵗ₀, prove it correct, and test it on the same candidates. It is
   the practical route for in-run evaluation and Phase D's baseline (b4). It passed at toy size on 2026-11-13
   ([`probe-candidate-evaluation.clj`](../../docs/tslc/probe-candidate-evaluation.clj)):
   a `Nat → Nat` candidate evaluated on 3 gives its real output, and a certificate of
   any other program gives the fallback. What remains is to apply it to the
   projections of agent candidates, at stated sizes.
6. **Documents:** an R4 section on agents, and a tutorial section.

**Success:**
- the theorem is kernel-checked;
- the runner's tests pass, red before green, including the existing probes' cases;
- the gate probe runs at stated sizes.

**Failure:**
- the theorem needs a rule change, or a metatheorem beyond the projection lemma; or
- the gate is infeasible at practical sizes.

Each is recorded with what the agent loses.

### Phase D — the experiment

- **Task and scorer:** candidate programs for a fixed task, scored by the agent's own
  fixed scorer on held-out inputs.
- **Proposer:** one untrusted stream of candidates, with their derivations, for every
  regime.
- **Regimes:**
  - (b1) adopt by check alone;
  - (b2) trial for a round, and revert by state;
  - (b3) hand over to a checked wrapper that runs the candidate and reverts;
  - (b4) evaluate in-run with the step-limited evaluator, then adopt;
  - (t) evaluate in-run with `reflect`, then adopt.
- **Metrics:** the score of the adopted program after N rounds; the number of rounds
  run under a worse program; tokens and time.
- **Hypothesis:** at equal tokens and time, (t) reaches a higher score than (b2), (b3)
  and (b4), or wastes fewer rounds.
- **Prediction:** it does not beat (b4). A pass would be a surprise to explain.
- **Pre-registration:** before any run, fix in this ADR:
  - the seeds of the candidate streams;
  - N, the per-round budget and the timing method;
  - the metric, the margin and the decision rule (consistent across seeds; a tie
    fails).
- **Refuted** if (t) fails that rule. A refutation is reported
  as the finding that self-justification adds nothing measurable to self-improvement
  in this architecture.

### Optional

- **Phase B — reflection beyond base types** (function types; `Π(x : D). T(b)` with b
  free of `reflect`). This is research on how far sound self-trust reaches.
- **Phase C — the time guard in the language:** ▷, `become` with premise `chk c ⌜A⌝`,
  the checker's cases, the reading by rounds, and termination. Take it up for the
  research aim of internal foresight.

### Scope note for ADR-0008

Extraction should cover the decoder, the evaluator and the runner path used by
`lc/run-agent`, not only `Check`.

## Order and resources

- **Phase A** can start now on its own worktree. It changes nothing that p2, s3 or x8
  work on.
- **Phase D** follows A's gate.
- **Lanes:** one Codex lane for the Ansatz work, within the memory rule. Claude does
  the runner, the probes, design and review. Starting a lane is the user's decision.

## Open decisions for the user

1. Accept this plan.
2. When to start Phase A.

## Not planned

- Unconditional internal knowledge of the lineage: shown impossible among the
  mechanisms considered.
- Gains in logical strength by self-certification: excluded by G2.

## After-action report

Pending.
