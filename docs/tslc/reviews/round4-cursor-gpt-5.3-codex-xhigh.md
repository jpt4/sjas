## Round-4 verdicts

- **(a) Verdict: partly holds.**  
  In principle, yes: `R4-certificate-calculus.md` says the ordinary layer has System‑T strength and unrestricted computation on `Syn`, and `R4-metatheory.md` gives `recNat`/`recSyn`/`caseLbl` plus a primitive-recursive `Check`. That is enough to encode a step-indexed evaluator relation.  
  But this interpreter is **not** currently built/proved in the evidence files, so this is capability-level, not established artifact-level.

- **(b) Verdict: holds (conditional on (a)).**  
  A fuel evaluator `run(c,x,k)` can take runtime `x`, avoid per-input derivation certificates, and enforce timeout by `k`. That matches your claim and contrasts with current `reflect` gate probes, which consume per-(candidate,input) derivations (`probe-candidate-evaluation.output.txt`).

- **(c) Verdict: holds.**  
  This is aligned with the docs: `RATIONALE.md` now says size buys predictability, not speed, and completion can be “however long the run takes.” `R4-certificate-calculus.md` also explicitly says no global polytime guarantee (System‑T strength). Token cost is an advance bound on **certificate size**, not a practical runtime bound.

- **(d) Verdict: partly holds.**  
  For many operational uses, yes, fuel is more flexible (any runtime input, explicit time cap, no per-input cert).  
  “Dominated” is too strong: `reflect` gives a total `D` result (no timeout branch) once accepted; that can matter for specs. And yes, at `⊥` this is `H`, which never runs (`R4-metatheory.md`, evaluator section / Theorem 5.2 discussion).

- **(e) Verdict: partly holds.**  
  Correct: the practically desired “use runtime universal proof directly” points toward reflection beyond base data; both `RATIONALE.md` and `ADR-0009-tslc.md` mark that as open (Phase B; Lemma 3.4 issue).  
  Overreach: “no practical capability” is too absolute; completeness/no-timeout can itself be practical, not only theoretical.

- **(f) Verdict: partly holds.**  
  Missing-baseline point is strong: Phase D in `RATIONALE.md` / `ADR-0009-tslc.md` has `(b1,b2,b3,t)` but no `(b4)` fuel-interpreter regime.  
  Predicted `(t) ≈ (b4) or worse` is plausible, not guaranteed: `reflect` may win if native evaluator path beats an in-language interpreter; fuel may win on cert overhead/timeouts.

**Plain verdict on (e):** **partly true, overstated**.

## Missed possible advantages of `reflect` over fuel (concrete)

- **No timeout branch in type/use-sites:** returns `D`, not `Option D`; simpler downstream logic when total answer is required.
- **Potential native-speed edge:** built-in evaluator path can be faster than self-interpreting code-level execution.
- **Trust-surface tradeoff:** if core evaluator/checker are already trusted/proved, adding a separate verified interpreter may enlarge proof/maintenance burden.
- **Audit semantics:** per-run certificate consumption/logging can be a feature when you want explicit proof artifacts tied to evaluations.
