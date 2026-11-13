# Artemov, Willard, selector proofs and λᶜᵉʳᵗ: what serves a self-improving agent

Dates: 2026-11-11 to 2026-11-12

Full record: [transcript](2026-11-12-artemov-willard-selectors-and-lambda-cert-transcript.md),
with every request and answer verbatim. This note states what stands, what was
withdrawn, and what is unchecked.
Builds on the [2026-10-29 selector note](2026-10-29-selector-proofs-and-guarded-trust.md),
the [2026-10-06 note](2026-10-06-artemov-lp-and-lambda-cert.md) and the
[trust-horizon note](2026-11-05-trust-horizon.md).

## Requests (in order; verbatim in the transcript)

1. Who commented on Artemov's work, showing it converges to previous results? Then:
   elaborate on the caveat, from the literature and direct sources.
2. Review a comment: what is lost, for programming, when the uniform consistency
   sentence is relaxed to a schema; must the antecedent of the relative-consistency
   inference, or the finite fragment, be determined empirically?
3. Artemov (as the user reports from a call): selector proofs internalize existing
   practice in computer-aided proving. Can they be made executable so that practice
   differs?
4. Find the deep difference between Willard and Artemov, and where λᶜᵉʳᵗ fits, for
   the driving question: what facilitates a reflective, self-modifying agent's
   self-improvement?
5. Evaluate λᶜᵉʳᵗ on that front. Can it be improved, or a better language written?

## 1. The commentary on Artemov

- **Gadsby 2025, Prop. 4.1.** T selector-proves Con(W) exactly when
  T ⊢ Con(T) → Con(W). Selector consistency is relative consistency. The corollary
  is credited to Ignjatović 1994.
- **Two directions.** The ceiling (left to right) uses only that the selected proofs
  exist, so it binds contentual selectors too. The floor (right to left) is built by
  explosion; Gadsby's Remark 4.2 calls those selectors non-contentual in Artemov's
  sense (Artemov 2024, §5.2).
- **Self-consistency.** With W = T the condition is Con(T) → Con(T). Every theory
  meeting the hypotheses selector-proves its own consistency; Grobner 2026
  (Cor. 2.9) shows the same by partial truth predicates on finite fragments.
- **Consequence (mine).** PA + ¬Con(PA) selector-proves its own consistency. No
  formal construction in these papers separates it from PA. Artemov's informal
  contentual requirement, belief in the axioms, carries the claim.
- **Other commentary.** Grobner places the selector in Mostowski's reflexivity
  theorem and Pudlák 1986. Artemov's "Non-Compact Proofs" takes Mostowski's proof as
  its example and keeps an interpretive claim: G2 forbids only compact proofs. Kripke
  2022 argues that Hilbert's method could not prove PA's consistency in PA; Artemov
  (2025) replies that this is premature.

## 2. Schema against sentence, for programs

- The scheme {¬n:⊥} holds in every model of T, including those with a nonstandard
  refutation. It certifies standard proofs; it does not exclude nonstandard ones.
- Hilbert's aim stays blocked: IΣ₁ cannot selector-prove Con(PA) (my derivation from
  Prop. 4.1).
- What the schema lacks is one step: from "s(x) proves ¬x:⊥" to "¬x:⊥" at a variable
  x. That is reflection, equivalent here to Con(T). So trust cannot be used about
  proofs not yet held: proof generators, plans for self-modification, D2 and D3.
- For any proof in hand nothing is lost; its certificate costs more as the proof's
  formula complexity grows.
- The finite fragment is computed from the derivation, not found empirically. PA
  proves Con(PA↾n) outright. What is held from outside is the whole theory's
  consistency: one contradiction refutes it, and no evidence verifies it, because the
  selector already proves every instance.
- The theory relies on that hypothesis whenever a proved universal claim is used for
  instances not checked. Each such use is a choice: pay for the check, or rely on it.
- Consistency in either form covers Π₁ claims (safety). Termination needs
  Σ₁-soundness: PA + ¬Con(PA) proves that a search for a PA-refutation halts.

## 3. Executable selectors

Run on a system's own proofs, the consistency selector computes nothing the
derivation's last line does not show. Three constructions do change practice (mine,
none implemented):

- **A kernel rule that skips checking.** From a proof of ∀x [s(x) : F(x)], conclude
  F(n) for a concrete n. It adds no theorems, shortens proofs, rests on consistency
  alone, and cannot be used at a variable.
- **A self-modification protocol.** A new kernel T′ ships with a translation of its
  derivations into the old kernel's, verified by the old kernel. The translation must
  be an explicit program and the claims acted on must be bounded; without both, the
  protocol admits T + ¬Con(T).
- **Proof-carrying actions.** Each action's proof is checked when the action arises.

## 4. Willard, Artemov and λᶜᵉʳᵗ₀ (the answer of 2026-11-12 00:26 UTC)

No system's proof of its own consistency is evidence of it, since an inconsistent
system proves it too. Systems differ in where the trust is recorded and what can use
it.

- **PA (Artemov).** It proves that every derivation has a certificate. The step from
  certificate to fact is taken outside, one derivation at a time. It can certify a
  faster checker, by an explicit translation it proves total.
- **Willard.** The consistency statement is a sentence of the system, so it can be a
  premise about proofs not yet seen: "no accepted claim is ever refuted" (2011,
  Thm 6.12), not "every accepted claim is true" (2001, Thm 7.2). A system can hold
  that sentence only if it cannot operate on its own proofs, so it cannot certify a
  faster checker.
- **λᶜᵉʳᵗ₀.** PA's function on free codes, Willard's on held certificates, with
  ordinary computation at full strength. It has no rule for changing its checker.
- **Steps of a self-modifying agent.** Better proof search needs a fixed checker. A
  faster checker needs a verified translation and adds no strength. A stronger
  checker is always a new assumption. Sanction of a successor has two grades: "acts
  only on what the checker accepts" (any checker proves it) and "and that holds"
  (needs consistency inside).
- **Facilitators.** Adopt changes at the first grade; give the checker's theory the
  strength to prove translations total; require explicit translations; bound every
  claim acted on; add strength as explicit levels, each asserting the consistency of
  the one below, with fall-back when a refutation names its levels; track reach
  against evidence; put consistency inside only if the agent must be self-contained.

## 5. λᶜᵉʳᵗ₀ as a facilitator, and the language to build (2026-11-12 21:51 UTC)

- **Verdict.** λᶜᵉʳᵗ₀ achieves self-justification without arithmetization (T1, T2,
  relative to Ansatz's metatheory; G2 for codes still goes through PA from named
  hypotheses). It gives a self-improving agent little.
- **Why.** Self-modification is outside it, in the runner. Trust descends on size, so
  a lineage's holdings shrink: the "telomere" of Yudkowsky–Herreshoff §3 and §4.2.
  `chk′` is opaque on open codes, so no proof transformation can be certified.
  `reflect` runs closed base-type programs with no input. H never changes a run.
- **Reason.** Trust in one's own proofs is consistent when what is trusted lies below
  the truster in a well-founded order. λᶜᵉʳᵗ₀ and Willard guard by size. Fallenstein's
  T_κ guards by time: any accepted proof, effective one step later, successors of any
  size, consistency only while the clock runs. Replacing oneself needs only the time
  guard.
- **Proposed calculus.** (1) A type ▷A, guarded recursion, and `become : □A → ▷A` on
  free codes, which is T_κ's axiom as a typing rule. (2) Tokens as income per step,
  with the induction on time, then budget. (3) `chk′` equal to a checker program over
  a named rule table, so checker changes can be certified. (4) Bets on stronger
  kernels as named assumptions over held certificates, with blame.
- **Role of self-justification there.** Results needed within a step, invariants that
  hold outright across changes of proof search, and a size bound on the damage of a
  false bet.

## 6. Follow-up: plain terms, ticks and tokens, bets, G2 for codes (2026-11-12 23:42 UTC)

The user found §5's proposal full of unexplained terms, asked how supplying ticks
differs from supplying tokens, asked whether G2 for codes is wanted, and required
that every paper known only from its abstract be read. The full reply is in the
transcript.

- **Plain terms.** An agent runs in rounds and names the program for the next
  round. The hand-over rule (`become`) says "the next round runs this code" for any
  accepted code, at no cost in tokens; its result (type ▷) cannot be used in the
  current round. Tokens are renewed each round. The language is given the checker's
  defining equations. Unproved assumptions are named.
- **When the hand-over rule is needed.** With a fixed checker no new rule is needed:
  the program outputs a replacement whose derivation it tested as code, the runner
  runs it, and that every round is well typed is proved once, outside. The rule is
  needed when the language itself is to derive that its successors are agents, and
  when a replacement brings a different checker accepted on a proof of its soundness
  (Fallenstein–Kumar §8).
- **Ticks and tokens.** A token is held and spent by the program, and the runner
  fixes the number. A tick is one hand-over; nothing supplies it, and its count
  appears only in the soundness induction. The outside supplies as much as before.
  What changes is that the program, not the runner, names the successor.
- **Bet.** An assumption acted on without proof because none is available, such as
  the consistency of one's own checker or of a stronger one.
- **G2 for codes.** It is Proposition 5 (`R4-metatheory.md` §6). It is wanted as a
  theorem: it gives `H°` its meaning, and it is the test each new rule must pass. A
  direct proof by Löb's argument is expected for a language that can reason about
  its own checker. It would not change λᶜᵉʳᵗ₀'s Proposition 5 or phase 2.
- **Read for this reply.** Fallenstein–Kumar 2015 (all); Spies et al. 2022 (all, not
  the supplement); Pudlák 1986 (§1, Theorems 3.1 and 5.5); Pudlák 2020 (§4);
  Yudkowsky–Herreshoff §4.2 again. Not obtained: Ignjatović 1994, Kreisel–Lévy 1968.
  Krajíček–Pudlák 1989: introduction only. The Milawa papers: not read.
- **What the reading changed.**
  - The guard in both tiling papers counts updates, not clock time.
  - Fallenstein and Kumar prove the reflection theorems in HOL4 and leave the
    self-replacing system as future work.
  - Later credits are earned one per program step; a rule granting more is unsound
    in the standard model (§6.2).
  - Pudlák's bounds are as recalled. The limit on a certified speed-up is his 2020
    Theorem 4.1 plus my transfer to checkers.

## Backtracks

- 11-11 20:16: the caveat was first stated as limiting Gadsby's collapse. It limits
  only the floor; the ceiling binds contentual selectors.
- 11-11 21:11 and 21:45: the user sent the first answer back ("Try again.") and
  rejected the second as couched ("Stop using this misleading, couching reasoning
  style"). The rewritten versions are 21:28, 21:47 and 21:54.
- 11-11 23:12: the claim that Artemov's and Willard's forms each support exactly one
  of the two steps was withdrawn at 23:32; the pairing was forced onto the question.
  The 23:52 answer was framed by its relation to the earlier ones; 00:26 stands.
- 11-11 22:42: the protocol admitted T + ¬Con(T) (repaired above). "Löb fails for □ₙ"
  (11-09) holds only at fixed n or with budgets paid at full size (Critch, Thm 3).

## Status

- **Read in sources.** Artemov 2024 §5.2; Gadsby 2025 §4; Grobner 2026; Artemov,
  "Non-Compact Proofs"; Critch Thm 3; Yudkowsky–Herreshoff §3, §§4.1–4.2.
  Ignjatović 1994 from its abstract only.
- **Mine, not machine-checked.** The IΣ₁ argument; the PA + ¬Con(PA) example; the
  three executable constructions; that a verified translation preserves theoremhood
  along a lineage; that Willard-type systems cannot prove a lengthening translation
  total; everything in §5's proposal (reading `become` as T_κ's axiom, consistency of
  the combined calculus, erased derivations, a transparent `chk′` keeping T1, G2
  inside the calculus, the horizon carrying to tokens).
- **Search results only, as of §5 (read since; see §6).** Fallenstein–Kumar 2015; Spies et al., "Later Credits"
  (2022); Krajíček–Pudlák as stated by Joosten.
- **Checks not run.** A Lean toy with a time index, `become` on an unbounded box and
  `con` on the size-bounded box; the adopt-only-if-proved comparison of a PA kernel
  with a self-verifying one.

## Sources

- Artemov, arXiv:1902.07404, arXiv:2403.12272, arXiv:2512.12892, arXiv:2508.20346.
- Gadsby, "Properties of selector proofs", arXiv:2509.19373.
- Grobner, "Uniformity of Consistency in Arithmetic …: Ein Märchen", arXiv:2605.00266.
- Ignjatović, "Hilbert's Program and the Omega-Rule", JSL 59(1), 1994.
- Critch, arXiv:1602.04184. Joosten, arXiv:2004.05431. Pudlák, arXiv:2007.14835.
- Yudkowsky and Herreshoff, "Tiling Agents for Self-Modifying AI, and the Löbian
  Obstacle", 2013 draft, https://intelligence.org/files/TilingAgentsDraft.pdf.
- Fallenstein and Kumar, "Proof-producing reflection for HOL, with an application to
  model polymorphism", ITP 2015, https://intelligence.org/files/ProofProducingReflection.pdf.
- Spies et al., "Later Credits: Resourceful Reasoning for the Later Modality", ICFP
  2022, https://plv.mpi-sws.org/later-credits/paper-later-credits.pdf.
- Pudlák, "On the length of proofs of finitistic consistency statements in first
  order theories", Logic Colloquium '84 (1986), https://users.math.cas.cz/~pudlak/fin-con.pdf.
- Krajíček and Pudlák, "Propositional proof systems, the consistency of first order
  theories and the complexity of computations", JSL 54(3), 1989.
