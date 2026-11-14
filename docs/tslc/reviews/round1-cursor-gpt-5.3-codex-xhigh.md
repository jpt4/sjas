## Findings (Issues)

- **TSLC-01 (critical)** — **Location:** `RATIONALE.md` §5.1 lines 283–284 and §5.2 lines 322–324; `R4-metatheory.md` lines 1372–1374. **Argument:** the lineage proof step says Theorems 4/4′ give runner output/code/verdict agreement, but 4′ is only for base data types, while `O := Σ(a:Nat).T(safe a)` is not a base data type; the current statement overreaches. **Fix:** restate theorem on base projections (action `Nat`, successor `Syn`, verdict `Bool`) and derive safety property of `O` separately from semantics.
- **TSLC-02 (major)** — **Location:** `RATIONALE.md` §0 claim 3 lines 39–41, §4.2 lines 197–199, §7 line 424; `R4-metatheory.md` lines 251, 548–550, 1059–1066. **Argument:** `reflect` is presented as running “any program,” but R4 proves certified evaluation only at base data result types; higher types are explicitly left open. **Fix:** qualify all “complete evaluation” claims to “held, checked certificates whose certified result type is base data.”
- **TSLC-03 (major)** — **Location:** `RATIONALE.md` §0 claim 5 lines 54–56; `2026-11-13-agent-probe.clj` lines 41–47 and 59–66; `ADR-0009-tslc.md` lines 17 and 106–111. **Argument:** “no external trusted runner in the loop” is false as written; probes and ADR still depend on an external runner that decodes, executes, and re-checks handover. **Fix:** narrow claim to “no extra trusted evaluator for the inner scored code path; lineage execution still uses a trusted runner.”
- **TSLC-04 (major)** — **Location:** `RATIONALE.md` §3 lines 164–166 and §2.3 lines 146–151. **Argument:** “exactly two options” is too strong; the real invariant is “must block diagonal construction or enforce well-founded descent,” of which size/time are two concrete instances used here. **Fix:** replace “exactly two options” with “two options implemented in this architecture.”
- **TSLC-05 (major)** — **Location:** `RATIONALE.md` §4.2 lines 205–214; `R4-metatheory.md` lines 427–428, 485, 643–646. **Argument:** the four-step diagonal argument uses an unproven comparison (“larger than p’s own certificate”); the core metatheoretic fact is strict overhead `m < ‖r‖` for the *reflected certificate*, not a comparison to the caller’s own certificate. **Fix:** rewrite proof around `m < ‖r‖` + “recursive call needs to rebuild same certificate size,” and remove the unneeded claim about p’s own certificate size.
- **TSLC-06 (major)** — **Location:** `RATIONALE.md` §5.2 lines 301–330; `2026-11-13-agent-probe.clj` lines 44–47; `ADR-0009-tslc.md` lines 106–113 and 123–124. **Argument:** lineage theorem statement omits key runner hypotheses: runner executes exactly decoded term, with declared token budget, with same `Check` relation, and passes correct literal arguments each round. **Fix:** make these explicit theorem hypotheses and add a runner-correctness lemma binding operational runner behavior to the formal evaluator/checker.
- **TSLC-07 (major)** — **Location:** `RATIONALE.md` §7 line 422; `ADR-0009-tslc.md` lines 10–11 and 112–113. **Argument:** “Verifiable adoption” is marked `[R][E]` via extracted `Check`, but ADR says extracted checker is pending and current checker is still test oracle; evidence does not yet match the claim. **Fix:** split status into “current checker executed” vs “extracted checker pending,” or downgrade status mark.
- **TSLC-08 (major)** — **Location:** `RATIONALE.md` §6 lines 395–414; `2026-11-13-tslc-walkthrough.md` lines 210–217. **Argument:** section 6 reads as if transfer to λᶜᵉʳᵗ₀ is established, but walkthrough explicitly says transfer items are not checked (including guarded-recursion machinery and higher-type reflect-with-inputs). **Fix:** mark §6 claims as toy-level evidence only and list transfer obligations inline.
- **TSLC-09 (minor)** — **Location:** `RATIONALE.md` §2.2 line 130; `2026-11-13-tslc-toy.clj` lines 558–565. **Argument:** the text cites `sNoSoundReading` for the contradiction construction, but the direct closed contradiction witness is `sViaQuine`; `sNoSoundReading` is the meta consequence. **Fix:** cite `sViaQuine` for derivation of `⊥`, and `sNoSoundReading` for semantic no-sound-reading corollary.
- **TSLC-10 (major)** — **Location:** `RATIONALE.md` §5.4 lines 366–377 and §9 lines 476–489; `probe-reflect-adoption.output.txt` line 2; `probe-supply-income.output.txt` line 1. **Argument:** plan success criteria understate known scaling risks (98,593-node cert, 34,922-node variant, prior 1.5GB exhaustion), which can block Phase A/D execution. **Fix:** add explicit engineering gates in Phase A: candidate-as-data invariant, supply-as-argument requirement, memory ceilings, and checker switchover criteria.
- **TSLC-11 (minor)** — **Location:** `RATIONALE.md` §2.1 lines 100–114 and 116–120. **Argument:** diagonal/fuel argument is basically right, but assumptions should be explicit: ability to represent self code as data, construct `u(x,x)`, and interpret across all candidate codes; without those, typed-representation escapes apply. **Fix:** add one sentence listing these assumptions before applying to λᶜᵉʳᵗ₀ context.

## Section 0 Claim-by-Claim Verdict

- **Claim 1 (Two walls):** mostly correct, but add explicit assumptions for the evaluation wall (TSLC-11), and tighten Löb witness reference (TSLC-09).
- **Claim 2 (Guard is the way through):** directionally right, but too absolute as stated (TSLC-04).
- **Claim 3 (Self-justification executable via reflect):** core idea right, but scope is overbroad (“any program”) and diagonal proof needs tightening (TSLC-02, TSLC-05).
- **Claim 4 (Safety without self-justification):** plausible and consistent with ADR, but only with explicit runner hypotheses (TSLC-06).
- **Claim 5 (What self-justification adds):** partially right; overclaims removal of external trusted runner and overstates evaluation scope (TSLC-03, TSLC-02).
- **Claim 6 (Limit is sharp):** generally defensible for this architecture.
- **Claim 7 (Plan):** order is sensible, but practical failure modes are under-specified (TSLC-10).

## Direct Answers To Your Questions 2–10

- **Q2 (Diagonal/fuel):** argument is correct in form; for λᶜᵉʳᵗ₀ it assumes ability to build/apply code expressions from free data and self-quote enough to form the diagonal.
- **Q3 (Löb via toy):** correctly applied in the toy fragment (`SUTm` + `conCode` + D2/D3 + fixed point); “trust in code at every type fails” is justified via `A = ⊥`; “common cause” framing is accurate at high level.
- **Q4 (Five ways / exactly two):** table is mostly useful, not complete as a literal taxonomy; “exactly two options” is too strong.
- **Q5 (`reflect` + diagonal):** `reflect` account matches R4 for held checked certs at base data; four-step diagonal argument needs formal tightening.
- **Q6 (`H` as reflect-0 / executable content):** defensible; `H := reflect₀` is explicit in R4. Caveat: `H₁` is additional non-executable strength.
- **Q7 (Agent type / lineage theorem):** agent type is supported by probes; lineage theorem skeleton is plausible but needs explicit runner and projection hypotheses.
- **Q8 (Time guard / Tκ):** toy account is mostly correct; transfer to full λᶜᵉʳᵗ₀ remains open.
- **Q9 (Capability honesty / verdict):** “safe” and “justified” are mostly honest; “verifiable” and “evaluated” need tighter scoping; verdict is reasonable if goal includes internal scored evaluation, not just safety.
- **Q10 (Plan scope/order):** order is reasonable (A then D, optional B/C), but needs explicit risk gates for checker dependency and derivation-size scaling.

## What I Checked And Found Correct

- `H` as `reflect` at empty type is explicit in the metatheory.
- `reflect` denotation and soundness use strict budget descent (`m < ‖v‖`) in the Reflect case.
- Runner budget discipline (“fixed per run, no mid-run growth”) is explicitly part of R4’s assumptions.
- Unprovability of code-level consistency forms (`Con′_ω`) and `H° → Con′_ω` is explicitly stated in R4.
- The toy’s Löb counterexample machinery (`SUTm`, `sViaQuine`, `sNoSoundReading`) is present and coherent.
- Toy time-guard conditionality (`sLaterBotSucc` / `sLaterBotZero`) and `become` meaning theorem are present.
- Probe outputs support adoption-by-check behavior and re-check agreement.
- Probe outputs support size-guard behavior under insufficient supply and reflect-based scoring behavior on accepted held-evaluation paths.

If you want, I can produce a tightened rewrite checklist section-by-section for `RATIONALE.md` (minimal textual changes that remove each issue while preserving structure).
