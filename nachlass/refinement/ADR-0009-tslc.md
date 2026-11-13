# ADR-0009 — TSLC: self-modifying agents in λᶜᵉʳᵗ

**Status.** Proposed 2026-11-13. Awaiting the user's decision.

**Branch.** Recorded on `f6-p5`. Each phase below gets its own branch when it
starts (`adr-0009-a`, `adr-0009-b`, …).

**Depends on.** [R4](R4-metatheory.md) §§1–5; [ADR-0006](ADR-0006-formalization.md)
(T1–T4 and Lemma 3.6 in Ansatz); [ADR-0008](ADR-0008-extraction.md) (the
extracted `Check`, which the runner must use). Phase C also touches ADR-0007
(P5).

## Context

λᶜᵉʳᵗ₀ derives its own consistency over the certificates it holds. It has no
way to replace itself: a run is one program at a fixed budget, and choosing the
next program is the runner's act, which the theory does not cover.

The analysis of 2026-11-12 and 2026-11-13 asked what a self-modifying agent needs,
and checked the answer on a toy in Ansatz
([note](../../docs/log/2026-11-13-tslc-walkthrough.md),
[toy](../../docs/log/2026-11-13-tslc-toy.clj)). The agent's needs:

- **W1.** Every action in every round is safe, whatever successors it adopts.
- **W2.** It decides each adoption with its own checker.
- **W3.** Inside the language, it can rely on what a successor it adopts will do.
- **W4.** It derives its own consistency (H₀, H₁) and is consistent.
- **W5.** It has W3 unconditionally, for successors given as code of any size.

The toy establishes:
- W1, as a theorem about the language proved outside it;
- W2;
- W3 in two forms: unconditional for successors the agent holds as certificates,
  up to its budget, and from the next round on for successors given as code;
- W4, unchanged from λᶜᵉʳᵗ₀.

W5 is impossible: trust in free code without the delay derives ⊥ by Löb's
argument. This ADR builds W1–W4 in the real calculus.

## Decision

Build TSLC in three phases plus experiments. Every phase follows the project's
rules:
- an ADR section before code;
- red tests before green;
- statements kernel-checked in Ansatz in the exact form R4 states them, with an
  axiom audit;
- results relative to the metatheory;
- an after-action report.

### Phase A — Agents and lineages (W1, W2)

No new trust: the agent's checker decides adoption, and lineage safety is
proved outside the language.

1. **The agent type, by name.** Add a type former `Agent(I, O)`, with I and O base
   data types. Its values are functions from I to an output in O paired with a
   successor: a code `c` together with `T(chk′(c, ⌜Agent(I, O)⌝))`.
   - Its constructor is handed the agent's own code, like the toy's `quine`.
   - This keeps λᶜᵉʳᵗ₀'s rule that self-reference is by name (draft §2.4).
   - The alternative is to define the type by the diagonal on codes, which needs
     substitution on codes and the codes of open types. It is the fallback.
2. **Metatheory.** Add the new cases to Lemmas 2.x, 3.6 and Theorem 4′. Then
   prove the **lineage theorem**: for every closed agent, every input sequence and
   every k, round k's output lies in O's semantic set. The proof is an induction on
   rounds over Lemma 3.6, Theorem 4′ (adequacy) and decoding (Lemmas 2.6–2.8). It
   reads round k at the budget its derivation declares.
3. **Ansatz.** Add the cases to the formal development and prove the lineage
   theorem in the kernel.
4. **Implementation.** Add a typer rule and an evaluator case, and a runner
   `lc/run-agent`. Each round, the runner:
   - runs the program at its declared budget;
   - checks the successor's code with the extracted `Check`;
   - decodes the code, records the round, and runs the successor with the tokens
     its derivation declares.

   Red tests first: an agent that keeps itself, one that hands over to a different
   agent, a rejected successor, and a renewed budget.
5. **Documents.** An R4 section on agents, and a tutorial section.

**Success:** the lineage theorem is kernel-checked for λᶜᵉʳᵗ₀ with `Agent`, and the
runner's tests are green. **Failure:** a T1–T4 case breaks with the new
constructor. Record it, and take the diagonal route.

### Phase B — Adopting a held successor (W3, unconditional)

1. **Metatheory.** Extend `reflect` from base data types to `Agent(I, O)` with
   base I and O.
   - The conjecture: the result keeps the certificate's footprint, and the decoded
     program runs on its input at budget n − (‖v‖ − m).
   - Lemma 3.4 fails at higher types in general. The proof must use that an
     agent's semantic set depends on the budget only through its outputs, which
     are base data.
   - This goes in R4 §4.8.
2. **Ansatz:** a Lemma 3.4′ for these types, and the Reflect case at `Agent`.
3. **Implementation and tests:**
   - an agent that holds a certificate of its successor and adopts it within the
     round;
   - the token cost, measured.

**Success:** `reflect` at `Agent(I, O)` is kernel-checked. **Failure:** a
counterexample to the conjecture. Record it, and fall back to certifying the
successor's output for each input separately, at the per-instance cost of
Theorem 4.6.

### Phase C — The time guard (W3 for code)

1. **Rules.**
   - ▷A, `next : A ⊸ ▷A`, and application under ▷.
   - Guarded recursion `fix : (▷A → A) → A`, at usage ω, with its unfolding
     recorded in derivations.
   - `become : Π(c :ω Syn). T(chk′(c, ⌜A⌝)) → ▷A`.
   - The checker's F7 cases for these rules, keeping strict overhead.
   - The runner hands ▷-values over to the next round, with their declared
     budgets.
2. **Consistency side.** Read ▷A as the one-element type, which is Yudkowsky and
   Herreshoff's κ = 0. Then T1, T2, T3 and P5 carry over, with one new case each
   in Lemma 3.6 and in P5's step 2.
3. **Meaning side.** The reading by rounds:
   - ▷A with r + 1 rounds left is A with r left;
   - functions are Kripke-style: a function at r rounds left works at every
     r′ ≤ r;
   - the induction is on rounds, then on budget.

   The theorem: a value of ▷A handed over is a value of A in the next round, for
   every number of rounds.
4. **Termination.** T4 again, with `next` and `fix` suspended until the next round.
5. **Ansatz:** items 2–4 in the kernel.
6. **Implementation:** the typer, the evaluator and the runner, with an example
   agent that relies under ▷ on what its successor will do.

**Success:** items 2–4 are kernel-checked, and the example runs. **Failure:** the
reading by rounds fails in the presence of tokens. Record it, and restrict ▷ to
token-free types.

### Phase D — Experiments: what each mechanism buys

1. **The adopt-only-if-proved comparison.** Generate a set of candidate
   successors. Run four agents on them:
   - (i) adoption by type only;
   - (ii) (i) plus held adoption;
   - (iii) (ii) plus reasoning under ▷;
   - (iv) (iii) plus an H₁ gate on held commitments.

   Record which candidates each accepts, the token costs, and any unsafe action.
   By the lineage theorem there should be none.
2. **A held commitment.** A lineage carries a held certificate of a commitment, and
   H₁ rules out any held certificate of its negation. Measure what this costs and
   what it excludes.
3. **Report:** a note in `docs/log` and an entry in `LOG.md`.

### Later, as separate ADRs

- A checker the calculus can reason about, so that an agent can adopt a faster
  checker by an internal proof.
- Bets: named assumptions about stronger checkers, with blame, which is the
  trust-horizon toy carried into λᶜᵉʳᵗ.

## Order and resources

- **Phase A** waits for ADR-0008's extracted `Check`. Until then, the current
  checker can serve as its test oracle.
- **Phases B and C** follow A and are independent of each other. B is smaller.
- **Phase D** can start in part after A, and runs in full after B and C.
- **Lanes.** One Codex lane for formal work at a time, within the memory rule: at
  most one warm server and one suite. Claude handles design, ADR updates and
  review. Starting a lane is the user's decision, since Codex time is shared with
  p2, s3 and x8.

## Open decisions for the user

1. Accept this plan, which changes the status to Accepted.
2. The agent type: by name (recommended) or by the diagonal on codes.
3. When to start Phase A: now on its own worktree, or after x8 or p2.

## Success and failure for the ADR as a whole

**Success** has five parts:
- agents run indefinitely with renewed budgets and checked hand-overs;
- the lineage theorem is kernel-checked for the real calculus;
- held adoption at first-order agent types is kernel-checked, or its failure is
  recorded;
- the time guard's consistency and meaning theorems are kernel-checked, or their
  failure is recorded;
- the experiment report exists.

**Failure** of a phase is recorded with what the agent loses without that
mechanism. **No phase aims at W5,** which is impossible.

## After-action report

Pending.
