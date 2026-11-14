# Round 1 review: Claude Opus subagent (2026-11-13), on RATIONALE.md as of commit 6fe398f

Verbatim report.

**Critical**
- C1 (§0.5, §5.3, §7 "Evaluated", §10): the probes never evaluate the candidate. Both reflect a closed Nat
  program `sc` supplied by the proposer and unrelated to `cand`, so the proposer chooses the score and adoption
  is the proposer's say-so. Behaviour on test inputs needs a certificate of cand's application, built in-language
  (ADR-0009 item 4), which was not built; it needs a derivation builder in λᶜᵉʳᵗ₀, and the certificate costs at
  least cand's derivation plus its literal. Fix: mark [O]; add a Phase-A probe in which the agent fixes the scorer
  s, the proposer supplies a derivation code, and the agent checks it at Nat and compares its root term with
  `s ⌜cand⌝` by recSyn equality before reflecting; this needs no derivation builder.

**Major**
- M1 (§4.2 steps 2–3): with the supply written in the program text, p's certificate exceeds its k tokens and
  p⌜p⌝'s more so; p can spend only its k tokens, so it never pays, even at top level: step 2 is false. With the
  supply from the runner, "x applied to x" must include an inner supply s; the inner copy can pay for p⌜p⌝s′
  with a smaller s′, so step 3 is false; p(⌜p⌝, r) is depth-bounded recursion, well-founded on ‖r‖. "No
  contradiction" is right, because ⟦reflect⟧ⁿ recurses to m < ‖v‖ ≤ n, so no equation holds at a single
  budget. Also unstated: p must build an application's derivation code in-language. Fix: replace the steps.
- M2 (§0.3, §4.3–4.4): (a) H° is derived because H is postulated; descent explains consistency, not derivation.
  (b) Self-consistency only in certificate form; Con′ is not derivable (Prop 4.8, P5). (c) H₁ is not a reflect
  instance and does not follow from H (Prop 4.10′). (d) reflect at D ≠ 0 is separable from H: it is bounded
  local reflection, not Willard self-justification. (e) "T2 = Willard's clauses" is [R] informal (R4 §3.9).
  Fix: "H is the ⊥-instance of a bounded local-reflection principle whose other instances execute; H₁ is a
  separate postulate."
- M3 (§5.1 vs ADR-0009): the rationale's agent type has `r :₁ R` and ⊗; the ADR's has no supply, ×, and runs
  "with the tokens the derivation declares". With a supply the runner must run at cap n ≥ m + ‖r‖ with the
  supply's tokens apart from the agent's m; at n = m every certificate built from the supply fails reflect's cap
  and silently returns the default. The probe works only because m = 0 and it runs at k. Fix: reconcile;
  specify Θ_{m+k}.
- M4 (§5.2 hypotheses): (i) the runner evaluates the term decoded from the checked code; the probe runner
  re-types the application with typing.clj, so the trusted base includes typing.clj and eval.clj, not only
  Check. (ii) n ≥ m + ‖r‖ with distinct supply tokens (Π₁ clause of V: j ≤ n − k). (iii) d is exactly the
  canonical ⌜Agent⌝ of a closed formed type. (iv) I base data, so literals lie in V₀(I). (v) The re-check and
  ⟦chk′⟧ are the same Check at which Lemma 3.6 was proved; the implementation's checker is not that checker
  until ADR-0008 lands. (vi) Theorem 4′ is stated only for derivable base-type terms; using it on projections
  needs derivations of the projection programs in Θ_{m+k} or the Π-clause of 4′'s relation, which is in its
  proof, not its statement. (vii) `safe` closed and free of reflect. Fix: state all; plan (vi) as proof work.
- M5 (§3, §8): (a) with a runner supply, the size guard is complete only if the runner supplies enough, as with
  runner-supplied fuel; the real difference is that the tokens needed (nodes of the certificate) are computable
  from the code in advance, the fuel needed is not. (b) ▷ gives trust, never an evaluated result. (c) The toy's
  codes are intrinsically typed closed terms, with no checker and no tokens, so "candidates as data: yes" for the
  time guard is unsupported. (d) Missing row: strength stratification (an interpreter for a weaker language);
  the size guard is a tower indexed by budget. (e) §8 rejects Willard for not composing proofs, but λᶜᵉʳᵗ₀ cannot
  compose certificates uniformly either (Prop 4.4, Thm 4.6, Cor 4.6′). Fix: revise rows; frame escapes as
  well-founded descent (size, time, strength) versus external trust.
- M6 (§7): coherence via H₁: H₁ never runs (Thm 5.2); Check rejects contradictory pairs for every code (Cor 3.7)
  with or without H₁; H₁ only lets a program drop dead branches, so a coherence experiment measures nothing.
  "Verifiable [R][E]" is [O] while extraction is pending. "Interpretable": 35k–99k-node derivations are
  auditable, not interpretable; reflect consumes its certificate, so "each evaluation has a certificate" needs
  the runner to log it; the type specifies only each round's output. Fix: "auditable"; re-mark.
- M7 (§0.1, §0.6): the [K] for "cannot trust its own proofs" is a toy theorem with a built-in fixed point; for
  λᶜᵉʳᵗ₀ the result is P5 [R]. "The limit is sharp… impossible" has no exhaustiveness theorem. Fix: mark [A],
  scoped to the mechanisms considered.
- M8 (§5.1 income): a successor's text can declare any budget m′, which the runner mints; per-round income limits
  nothing. Fix: a runner policy capping m′ + k, or charging m′ to the supply.
- M9 (§9): (a) failure criterion should include "needs a new metatheorem" (M4 vi). (b) The C1 probe should be a
  feasibility gate before Phase D. (c) Every round re-types a term containing the agent's code as a literal
  (12–35 s per three toy rounds, 1.7 GB); the runner must evaluate the decoded derivation directly with literal
  values in the environment. (d) Phase D lacks task, generator, metric, baseline, falsifiable threshold
  (AGENTS.md rule 4); the baseline should be "trial for a round, then hand back", which may make reflect
  unnecessary since every checked candidate is safe. (e) Extraction must cover the evaluator and runner.

**Minor**: m1 the diagonal wall is for total languages. m2 u : Syn → Syn → Nat; fuel F(code, input); the fuel
argument assumes an internal fuel interpreter for all of λᶜᵉʳᵗ₀ and covers only fuel computed inside. m3 "never
holds more nodes than tokens" holds operationally only for the erasing evaluator in runtime positions (cite Thm
4′). m4 "§6.2" does not exist; "cannot be known inside" holds for code successors, not held ones. m5 O :=
Σ(a :ω Nat) rules out token-computed actions; use Σ(a :₁ Nat) or rebuild; no probe exercised T(safe a). m6 a
literal is recorded at every ancestor node. m7 "never certificates": the supply is one; say "never across
rounds". m8 the K row conflates reflection with consistency. m9 reflect runs inside the trusted evaluator; say
"no round-trip outside the run".

**Not checked**: the [C] citations; the agent-probe figures (no log).

**Checked and correct**: H := reflect₀ is R4's definition; §4.2's description of reflect matches R4 §3.2 and
eval.clj; §4.3's sketch matches Lemma 3.6's H₁ and Reflect cases, Lemmas 2.4 and 2.7, §3.6; a runner may hand in
an arbitrarily large supply without paradox; the agent type is well formed and Lemma 3.6 gives b = tt ⇒
Check(c, ⌜Agent⌝) = tt; §2.1's diagonal applies to any u : Syn → Syn → Nat in λᶜᵉʳᵗ₀; the probe figures match
the logs; §0.4 and §8 honestly say safety is ordinary proof-carrying code.
