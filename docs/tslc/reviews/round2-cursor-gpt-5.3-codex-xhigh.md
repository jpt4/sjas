## Round-1 Findings: Resolution Status

### From `round1-cursor-gpt-5.3-codex-xhigh.md`

| ID | Resolved? | Reason |
|---|---|---|
| TSLC-01 | **partly** | Scope is corrected (projection issue is now explicit and open in §5.2), but the proof sketch still mixes “done” phrasing with an unresolved lemma dependency. |
| TSLC-02 | **yes** | `reflect` is now scoped to held, checked **base-data-type** certificates; higher-type soundness is marked open. |
| TSLC-03 | **yes** | The “no external runner” overclaim is removed; trusted runner role is explicit. |
| TSLC-04 | **yes** | “Exactly two options” was replaced by a broader escape taxonomy and non-exhaustiveness caveat. |
| TSLC-05 | **yes** | Diagonal discussion now uses strict descent/supply-bounded recursion, not the old unproven certificate-size comparison. |
| TSLC-06 | **partly** | h1–h5 were added, but runner/argument invariants are still not fully explicit at theorem level (details in issues below). |
| TSLC-07 | **yes** | “Verifiable” was downgraded/split appropriately; extracted-checker status is now explicit. |
| TSLC-08 | **yes** | §6 now clearly says toy-only and lists transfer obligations. |
| TSLC-09 | **yes** | Contradiction witness is now properly tied to `sViaQuine`, with `sNoSoundReading` as consequence. |
| TSLC-10 | **partly** | Engineering gates were added, but concrete thresholds/criteria are still underspecified. |
| TSLC-11 | **yes** | Diagonal assumptions are now scoped to the total-language setting and stated more carefully. |

### From `round1-claude-opus-subagent.md`

| ID | Resolved? | Reason |
|---|---|---|
| C1 | **yes** | Candidate-evaluation claim is now marked open; Phase-A gate is introduced. |
| M1 | **yes** | Wrong diagonal steps were replaced by supply/budget descent framing. |
| M2 | **yes** | `H`, `H₁`, and `reflect` are now cleanly separated and correctly scoped. |
| M3 | **yes** | `r :₁ R`, tensor output, and `Θ_{m+k′}` runner model are aligned in rationale/ADR. |
| M4 | **partly** | Most hypotheses are now present, but theorem-side runner invariants are still not fully pinned down. |
| M5 | **yes** | §3/§8 framing was corrected: predictability vs fuel, toy-vs-design split, strength row, and non-uniform composition caveat. |
| M6 | **yes** | Coherence/interpretability overclaims were removed; auditability is now the right claim. |
| M7 | **yes** | Scope is narrowed; no exhaustiveness theorem is claimed. |
| M8 | **yes** | Runner budget policy for successor-declared budgets is now explicit. |
| M9 | **partly** | Phase structure/gates improved, but Phase D still needs tighter pre-registered decision criteria. |
| m1 | **yes** | Diagonal wall now clearly tied to total-language setting. |
| m2 | **yes** | Fuel argument now states “interpreter covers whole language” requirement. |
| m3 | **yes** | Runtime size claim now cites erasing-evaluator path (`Theorem 4′`) appropriately. |
| m4 | **yes** | “Known inside” scope is now limited correctly to code successors vs held certificates. |
| m5 | **partly** | `O` usage was fixed (`:₁`), but no probe yet exercises `T(safe a)` specifically. |
| m6 | **yes** | Literal-at-ancestor-node cost statement is now explicit. |
| m7 | **yes** | Cross-round certificate wording is corrected by supply-policy/log framing. |
| m8 | **yes** | Reflection vs consistency are no longer conflated. |
| m9 | **yes** | Trusted evaluator/runner boundaries are now stated more accurately. |

---

## New / Remaining Issues in Revision 2

| ID | Severity | Location | Argument | Fix |
|---|---|---|---|---|
| R2-01 | **major** | `RATIONALE.md` §5.3 gate; `ADR-0009-tslc.md` Phase A item 5 | Gate says “candidate term applied to input literal,” but the §5.1 agent interface has extra dynamic arguments (`d`, `self`, supply/state). As written, the certified term to be reflected is under-specified. | Define one canonical target term for gate checking (full application + projection), and use that exact term in both prose and probe. |
| R2-02 | **major** | `RATIONALE.md` §5.3 (root-term check) | Root-term equality alone is not enough to conclude “this cert evaluates the candidate on that input” unless you also fix/extract the right root judgment components (context/budget/literals) and prove extractor correctness. | Add explicit lemmas: root extraction correctness from checked derivation code, plus gate-soundness theorem tying `Check + root-eq (+ context/arg checks)` to reflected execution of the intended term. |
| R2-03 | **major** | `RATIONALE.md` §5.2 (h2–h3) | h2/h3 still do not explicitly enforce per-round argument discipline (`d = ⌜Agent⌝`, `self = current code`, transition to exactly returned successor on true verdict). The theorem conclusion depends on these invariants. | Strengthen hypotheses (or add h6/h7) with these per-round runner invariants. |
| R2-04 | **major** | `RATIONALE.md` §5.2 proof sketch | The text still says Theorems 4/4′ align runner/model values, while the projection path needed for that use is explicitly open. | Rephrase step as conditional and keep lineage theorem “pending” until projection + runner-correctness lemmas are finished. |
| R2-05 | **major** | `RATIONALE.md` §9 and `ADR-0009-tslc.md` Phase D | Phase D is better, but still not fully falsifiable operationally: no fixed seed protocol/threshold/decision rule for “beats b2/b3 at equal tokens/time.” | Pre-register candidate stream(s), round budget/time accounting, and a concrete pass/fail rule (effect threshold and tie handling). |
| R2-06 | **minor** | `RATIONALE.md` §8 and §9 baselines | b2/b3 are safe without `reflect` only under explicit preconditions (check-gated candidates/wrappers, reversible state model). Also “only escape” wording is still broad without a “full-language candidate” qualifier. | Add explicit safety preconditions for b2/b3 and tighten the “only escape” scope statement. |

---

## Plain Verdict

Revision 2 is a substantial improvement and is mostly well-scoped now. The major round-1 overclaims were largely corrected.

It is **not yet fully acceptable as a complete, well-founded end-to-end account** for a skeptical expert. Main blockers are:  
1) a precise and proved gate-soundness story for “evaluate candidate itself,”  
2) stronger runner invariants in the lineage theorem statement/proof,  
3) closure of the projection/use-of-4′ gap, and  
4) a fully pre-registered/falsifiable Phase D protocol.

---

## What I Checked and Found Correct

- `reflect` scope is now correctly limited to base data types, with higher types explicitly open (`R4-metatheory` + revised §4.2/§10).  
- `H`/`H₁` treatment is now correct: postulated constants, safety by descent, and non-derivability distinctions are properly scoped (§4.3/§4.4; `R4-metatheory`).  
- Probe evidence matches revised claims:  
  - `agent-probe.output.txt`: adoption verdict vs runner re-check agreement and lineage behavior.  
  - `probe-supply-income.output.txt`: supply-as-argument behavior and insufficient-supply fallback.  
  - `probe-reflect-adoption.output.txt`: reflect used in decision, but still not candidate-behavior evaluation (now correctly marked open).  
- ADR/rationale alignment improved on `Θ_{m+k′}`, runner direct decoded-eval path, budget policy, and expanded extraction scope.
