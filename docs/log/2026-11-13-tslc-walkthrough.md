# What TSLC gives a self-modifying agent: the reasoning, checked

Date: 2026-11-13

Check: Ansatz, [`2026-11-13-tslc-toy.clj`](2026-11-13-tslc-toy.clj). It uses the
method of the [guarded-trust](2026-10-29-guarded-trust.lean) and
[trust-horizon](2026-11-05-trust-horizon.clj) toys. Background: the
[2026-11-12 note](2026-11-12-artemov-willard-selectors-and-lambda-cert.md), §§5–7.
Build plan: [ADR-0009](../../nachlass/refinement/ADR-0009-tslc.md).

## Requests (verbatim)

> Let's plan out the entire build. Also, it is not clear that TSLC does what we
> want it to, and supply self-improving agents with self-justification for safe
> self-modification. Walk through the reasoning for TSLC is the detail necessary
> to verify its result, but also demonstrate their validity.

> Also, use Ansatz and Clojure, not Lean.

## The answer

TSLC does not give an agent an internal, unconditional proof that adopting a
successor found as code is safe. No consistent language in which code is free can
give one (Theorem G). What TSLC does give:

| What the agent needs | In TSLC | How | Checked in the toy |
| --- | --- | --- | --- |
| Every action in every round is safe, whatever successors it adopts | yes | a theorem about the language, proved outside it by induction on rounds | `sLineageSafe` |
| Adoption decided by its own checker | yes | a successor's code must check at the agent type | the agent type |
| Trust, inside, in a successor it holds as a certificate | yes, unconditional, within the budget | reflection at the agent type on a held certificate | `reflA`, `sAdoptHeldRun` |
| Trust, inside, in a successor given as code of any size | from the next round on only | the hand-over rule `become` | `sBecomeSpec`, `sLaterBotSucc` |
| Its own consistency: H₀ and H₁ | yes | unchanged from λᶜᵉʳᵗ₀; now about the logic every round runs | `sSelfJustifying` |
| Unconditional trust, inside, in code it has found | impossible | Löb's argument | `sNoSoundReading` |

So self-justification reaches self-modification in one place: what the agent
holds. Certificates it holds are trusted unconditionally, up to its budget. Code
is trusted only from the next round on. The safety of the whole lineage is a
theorem about the language: the agent's checker applies it, and the agent does
not prove it.

## Terms

- **Round:** one run of an agent: it reads its input, emits an action, and names
  the program for the next round.
- **Lineage:** the sequence of programs an agent and its successors run.
- **Budget:** the number of tokens a round holds. In the toy, size stands for it.
- **Held certificate:** a proof the round has paid for and holds. Only these
  count as self-justifying evidence.
- **Code:** a proof written as data; free to build, copy and compute with.
- **Later A (▷A):** A from the next round on. A value of type ▷A cannot be used in
  the current round.
- **`become`:** the hand-over rule, from a code that the checker accepts at A to
  ▷A.
- **Reading:** the model. Each type is read as a set, at r rounds left and budget
  n. A syntax is consistent if some reading sends ⊥ to the empty set.

## The toy

**Types:** `A → B`, `held A`, `code A`, `later A`, `⊥`, `godel`, `agent`, `safe`.

**Terms:**
- variables, λ and application;
- `quoH t`: a held certificate of a closed term t, of size `size t + 1`;
- `quoC t`: the code of a closed term t, of size 1 whatever t is;
- D3 and D2 on codes, at a variable: `iquoC : code A → code (code A)` and
  `iappC : code (A → B) → code A → code B`;
- `fold` and `unfold` for the Gödel sentence G ≅ (code G → ⊥);
- H₀, `con : held ⊥ → ⊥`, and H₁, `con1 : held A → held (A → ⊥) → ⊥`;
- `become : code A → later A`;
- `reflA : held agent → agent`;
- actions `act0`, `act2` of type `safe`;
- `quine f g : agent`, an agent handed its own code: f gives its action and g the
  code of its successor;
- `actOf`, `succOf`, and `abort : ⊥ → A`.

**Reading at r rounds left and budget n:**
- `A → B` is the functions from A's set to B's;
- `held A` is the closed terms of A of size below n;
- `code A` is all closed terms of A;
- `later A` is one element when r = 0, and, when r = s + 1, a budget m together
  with an element of A read at (s, m);
- `⊥` is empty, `godel` is the functions from closed terms of G to the empty set,
  `agent` is a safe action paired with a closed agent term, and `safe` is the even
  numbers.

## The theorems and their proofs

**Lemma (soundness).** For every r and n, every term of size at most n has a reading
at (r, n) in its type's set. Proof by induction on r, then on n, then on the term.
The cases that use the induction hypothesis:
- `con`: a held refutation s has `size s < n`. Its reading at (r, size s) lies in
  the empty set by the hypothesis for smaller budgets, so the case never arises.
- `con1`: held certificates of A and of A → ⊥ both have size below n, so both fit
  at budget n − 1. Read both there and apply one to the other: the result lies in
  the empty set.
- `become` on a code c, of any size: with no rounds left, the one element. With s + 1
  left, the pair (size c, reading of c at (s, size c)); the hypothesis for fewer
  rounds covers every budget.
- `reflA` on a held agent a: read it at (r, size a). An agent's set does not depend
  on the budget, so the result is an agent at n.
- `quine f g`: (reading of f at the term itself, reading of g at the term itself).
  The term is closed, so it is its own code.

H₀, H₁ and `reflA` look at a smaller budget in the same round. `become` looks at
the previous round, at any budget. Rounds first, then budget, serves both; the
other order fails at `become`, whose budget is unbounded.

**Theorem A (consistency; `sConsistent`).** There is no closed term of ⊥: its reading
would lie in the empty set. The reading with no rounds left already suffices, and
there `later A` has one element.

**Theorem B (self-justification; `sSelfJustifying`).** `con` and `con1` are closed
terms, and by A the syntax is consistent. These are the two clauses of Willard's
definition: the system derives a statement of its own consistency, and it is
consistent.

**Theorem C (what `become` means; `sBecomeSpec`).** With s + 1 rounds left, the
reading of `become` applied to a code of t is the size of t together with t's own
reading with s rounds left. The kernel checks this by computation.

**Theorem D (the time guard is conditional).** "Falsehood next round", `later ⊥`, is
empty while rounds are left (`sLaterBotSucc`). It has an element when none are
(`sLaterBotZero`, refusal 2). The time-guarded H₀ and H₁ are closed terms:
- `sTgH0 = λc. become c : code ⊥ → later ⊥`;
- `sTgH1 = λc d. become (iappC d c) : code A → code (A → ⊥) → later ⊥`.

With no rounds left they hold whatever the syntax proves, so they assert no
consistency. With a round left, the time-guarded H₀ says there is no code
refutation (`sTgH0Meaning`), but only the reading, outside the syntax, establishes
it. This is T_κ's theorem κ > 0 → Con(T_κ): its axiom at φ = ⊥.

**Theorem E (lineage safety; `sLineageSafe`).** For every closed agent term a and every
k, the action of a's lineage in round k is even. Proof:
- Round k runs a closed agent term a_k. Its reading at (0, size a_k) is a pair: an
  action, even by the reading of `safe`, and a closed agent term a_{k+1}.
- Each round is read at its own size, so the budget is renewed every round.
- The proof uses no time guard: the reading with no rounds left suffices.

`sModifyRun` runs an agent that acts 0, then hands over to a different agent that
acts 2: the kernel computes the actions of rounds 0 and 3.

**Theorem F (adopting a held successor; `reflA`).** The `reflA` case above:
`sAdoptHeldRun` runs an agent that holds a certificate of its successor and acts as
that successor does, from round 0.

**Theorem G (the limit; `sNoSoundReading`).** Take the part of the syntax that handles
codes and add `conCode : code ⊥ → ⊥`, consistency over codes. This is the case
A = ⊥ of trust in code without the delay. With G ≅ (code G → ⊥):
- `h := fold (λc. conCode (iappC (iappC ⌜unfold⌝ c) (iquoC c)))`;
- `unfold h ⌜h⌝ : ⊥`.

So no reading of that syntax sends ⊥ to the empty set. This is Löb's argument, the
2026-10-29 toy's `viaQuine` on codes. Trust in code at every type includes this
case, so it is inconsistent; with the delay it is `become`.

**Refusals.** The kernel refuses three statements:
1. that a code's size bounds what it holds;
2. that falsehood next round is refuted with no rounds left;
3. that H for the next round, `held (later ⊥) → ⊥`, can be read by the step that
   reads H₀.

**Axioms.** None of the theorems above reaches an axiom. On 2026-11-13 the file
loaded in a fresh JVM in 36 s at 600 MB, printing `TSLC-TOY-OK`.

## From the toy to TSLC

1. **Tokens.** The toy has none: size stands for budget, and H₁ reads both
   certificates one budget lower. λᶜᵉʳᵗ₀'s semantic types count tokens, so its H₁
   needs both certificates to share one budget, m₁ + m₂ < n, which affinity gives.
   That case is already kernel-checked (T2, ADR-0006).
2. **The checker.** The toy's codes are syntax with D2 and D3 built in. In λᶜᵉʳᵗ₀,
   `become`'s premise is `chk′(c, ⌜A⌝) = tt`, and decoding (Lemmas 2.6–2.8) turns
   an accepted code into a derivation.
3. **`next`, application under `later`, and guarded recursion** are not in the
   toy. In the reading with rounds, a function at r rounds left must also work at
   every r′ ≤ r (the standard step-indexing device).
4. **Inputs.** The toy's agent takes none, so `reflA` reads data. A real agent is a
   function of its input, and λᶜᵉʳᵗ₀'s `reflect` is proved only for base data
   types: Lemma 3.4 fails at higher types. Extending it to first-order types with
   inputs is a conjecture.
5. **The runner.** A round's budget must count every certificate the round holds,
   including any handed over to it, or H₁'s budget argument fails.

## Why the delay is needed where consistency does not need it

Trust in code without the delay, at the agent type alone (`code agent → agent`), is
consistent: a reading that ignores the code and returns a fixed agent satisfies it.
No reading runs the code, though. An agent can apply it to its own code, and that
code's reading then needs itself. The delay orders runs by rounds, one round per
hand-over. So the time guard is what makes an agent's statements about its
successors true of the successors that actually run; consistency does not need it.
This remark is mine and is not checked.

## What this gives a self-improving agent

- **The type as the invariant.** Adoption is a decidable check, and lineage safety
  is a theorem about the language. This needs neither self-justification nor the
  time guard.
- **Held adoption.** Unconditional trust, inside, in successors and lemmas it has
  paid for, up to its budget.
- **Reasoning about successors.** Under `later`: unbounded, and conditional on
  there being a next round.
- **Coherence.** By H₁, no two certificates it holds prove a type and its
  negation, however its proof search has changed.
- **Not available:** unconditional trust in found code (Theorem G), and gains in
  logical strength, which are assumptions adopted without proof.

## Status

- **Checked in Ansatz:** Theorems A–G, the three refusals and the axiom audit, for
  the toy.
- **Not checked:**
  - the transfer to λᶜᵉʳᵗ₀ (items 1–5 above);
  - Kripke-style `next`, application under `later` and guarded recursion;
  - `reflect` at first-order types with inputs;
  - the remark on the delay.

## The five items, reassessed (later on 2026-11-13)

The user asked whether the five items above could be done sooner, in fewer steps.
None needs a change to λᶜᵉʳᵗ₀ for safe self-modification. The probe
[`2026-11-13-agent-probe.clj`](2026-11-13-agent-probe.clj) runs the agent layer in
the current implementation.

1. **Tokens.** H₁'s shared budget is already kernel-checked (T2).
2. **The checker.** The agent type
   `Π(d :ω Syn). Π(self :ω Syn). Π(i :ω I). O × Σ(c :ω Syn). Σ(b :ω Bool). T(if b (chk c d) tt)`
   uses existing rules only. The agent checks its candidate with `chk` at run
   time, and the runner ties the knot by passing ⌜Agent⌝ and the agent's own code.
3. **`next`, application under `later`, guarded recursion.** These are needed only
   for static reasoning about the unbounded future. A goal that spans rounds
   becomes an invariant on passed state, and the lineage theorem covers all
   rounds. Deferred.
4. **Inputs.** Trial adoption needs no held adoption, since any candidate that
   checks at the agent type is safe to run. `reflect` at base types already
   serves certified use per input; extending it is an optimization.
5. **The runner's count.** Hand codes across rounds, never certificates.

**The probe's results.**
- The agents type-check, with derivations of 1,819 and 1,751 nodes.
- The agent's own verdict and the runner's re-check agree on a real successor, on
  junk, and on a type code.
- A decode-and-run lineage gives the outputs 0, 2, 2, 2, and stops on junk.
- It took 13 s at 174 MB.

**The constraint found.** A code literal in program text is recorded again at
every node of the derivation, and certifying such a program exhausted a 1.5 GB
heap. Candidates travel as data.

**Consequence.** ADR-0009's Phase A is the whole agent layer. Its lineage theorem
is an induction over Lemma 3.6, CheckSpec and Theorem 4′. Phases B and C are
optional.
