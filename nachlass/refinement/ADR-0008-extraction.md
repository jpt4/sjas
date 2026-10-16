# ADR-0008 — Extract the λᶜᵉʳᵗ₀ trusted core

**Status.** Accepted 2026-10-14. Implementation in progress on `adr-0008-extraction` (step 1's baseline recorded 2026-10-16); after-action report pending.

**Branch.** This decision is recorded on `adr-0006-complete`. Task `x8` implements it on a new `adr-0008-extraction` branch.

**Depends on.** [R4](R4-metatheory.md) §§1.6, 4.11–4.12 and 5; [ADR-0006](ADR-0006-formalization.md), especially F5/F7 and its trust base; the [statement audit](../docs-statement-audit.md).

## Context

The formalization has a concrete canonical F7 checker, but the reference program still duplicates its encoding, checker and evaluator in hand-written Clojure. Its `lc/run` type-checks with the hand-written typer, then runs the erasing evaluator without producing or checking a certificate (`code/lcert/src/lcert/core.clj:54`). The paper's measured sizes use that different reference encoding. This is observable drift in a largely machine-generated project with an unnecessarily large audit surface.

## Decision

1. **Extract the core.** Every operation that can decide certificate acceptance or program behavior in the production path is generated from an Ansatz kernel-checked definition, using `ansatz.codegen/ansatz->clj` and `ansatz.codegen.storedef/compile-store-def!` (`a/defn` uses this route). The formal sources are:

   | Component | Source or required functionalization |
   | --- | --- |
   | `Exp`, `Code`, usage and judgment data | `formal/lcert/formal/syntax.clj`, `skel.clj`, `usage.clj`, `judgment.clj` |
   | `encE`, expression decoding and F7 `encCert` | `encode.clj`, `certenc.clj`, `certcanon.clj` |
   | Canonical `decCert`, `Check decCert`, its rule checks and recursive fuel | `certcanon.clj`, `check_spec.clj`, `check_dt.clj`, `check_der.clj`, `check_skj.clj`, `check_hd.clj` |
   | `nodes`/`budget` and size guards | `cnodes` in `skel.clj`; decoded root budget and guards in `check_spec.clj`/`certcanon.clj` |
   | Erasure and budgeted evaluation, including `reflect`, `inspect` and recursive checking | `Er`/`EvalE` in `erase.clj`, `Eval` in `eval.clj`, Theorem 4′ and Theorem 5.2 proofs |

   `Er`, `Eval` and `EvalE` currently describe relations. Define executable functions on the *selected checked derivation* and runtime data, prove their results coincide with those relations on accepted inputs, and extract the functions. Where the paper defines a function, keep the formal definition extractable when feasible. Prove both directions wherever the relation is functional; if it permits multiple results, first make the paper's observation and the exact formal statement agree, then prove the appropriate observational equality. In particular, the extracted `reflect` path must use the extracted checker and the same checked derivation's erasure. A relation alone is not executable code.

2. **Keep the shell untrusted.** EDN parsing, surface syntax, the certificate-producing typer, the REPL and printers may remain hand-written. No shell result runs until its F7 certificate passes the extracted `Check`. A malformed shell result may be rejected; it cannot make `Check` accept an invalid derivation. The trusted runner must:
   - **Run what was checked:** decode the accepted certificate and execute that term, never the surface term passed to the typer. Return the checked judgment `(term, type, budget)` with the result. Compare the decoded term with the parser's output in tests and reject a mismatch in the production runner using extracted `Exp` equality.
   - **Check the caller's claim:** take expected closed type `A` and budget `n` from the caller, call `Check(c, encE(A))` rather than using the typer's reported type, and require the decoded root context to be exactly `Θₙ` using extracted data comparisons. The runner reports the decoded judgment before exposing the result, so a different well-typed term is visible. The compatibility API must not silently substitute the typer's type for the caller's expectation.
   - **Erase only checked code:** the evaluator that omits `reflect`'s runtime check under Theorem 5.2 receives only code whose certificate passed `Check`; nested `reflect` follows the proved extracted path. A checked outer program alone does not license an unchecked nested program.

   Preserve `lc/run`, `lc/check-program`, `lc/certify`, `lc/certificate-form` and the examples on this pipeline. The execution entry point takes `(n, expected-type, form)`. The existing two-argument `lc/run` remains for forms with a caller-written top-level `(the A t)` ascription; an unannotated call must obtain an expected type from the caller before it executes. Update examples that need an ascription or use the type-explicit entry point. Do not leave a route through the old evaluator. A later optional phase may formalize the typer and prove its certificate output sound, then extract it too; this costs a formal surface elaborator, derivation construction, conversion search and their correctness proofs.

3. **Use one encoding.** F7 in `formal/lcert/formal/certcanon.clj` is the canonical certificate encoding, produced by the extracted encoder. Retire the reference implementation's distinct encoding. Add F7's concrete constructor/label layout and decoder to R4 §1.6 as the canonical instance of E1–E6. Re-measure Proposition 4.11's four size pairs and Proposition 4.12(1)'s `414/460/522` under F7, including both terms' certificates in Proposition 4.11. Check *each* new count in the Ansatz kernel, for example by `decide` on `cnodes` of the generated F7 code, before replacing the paper's numbers. *Update (user decision, 2026-10-15):* phase 1 (task `afix`) now proves these counts in the kernel, with exact closed forms (`prop411size`, `prop412size`), and replaces R4's tables. This ADR then only adds a differential test that the extracted encoder reproduces those kernel-checked counts. Re-measure `TUTORIAL.md` numbers by the same method. Do not prove or retain the old encoding's counts as F7 facts.

4. **Match paper and proof.** For every `weaker`, `stronger` or `different` verdict in `docs-statement-audit.md`, either add the exact-form kernel statement (normally a specialization at `Check decCert`) or amend R4 to the precise proved claim. Record each choice and reason here in the implementation AAR and in `LOG.md`. Treat the remaining `open` claims as explicit obligations or amend their paper wording; no silent paper/formal gap is accepted. This includes Proposition 4.11's acceptance and growth claims and Proposition 4.12(1)'s polynomial bound, distinct from checking sample counts. Theorems about consistency remain relative to the Ansatz/Lean metatheory.

5. **Retain the public behavior and retire duplicate trust.** Differential tests compare the extracted core with the old implementation on every existing example and test program, after translating their certificates to F7; include rejected inputs, decoded judgments, erasure and results. Keep the hand-written core only as a test oracle until the comparison is complete, then delete it; retain recorded fixtures for regression tests. Preserve the public functions and example outcomes, while updating example type ascriptions where the caller's expected type must be explicit.

## Extraction spike and order of work

The 2026-10-14 code-generation probe found four concrete obstacles: most formal functions use `b/kdef!`, which kernel-checks **without generating Clojure**; `encE` has reducibility hints `-1`, so `compile-store-def!` skips it; forcing compilation raised an undiagnosed Clojure `CompilerException`; and `ansatz->clj` on `(encE Exp.tBool)` emitted `(encE [2])`, calling a function never generated.

1. **Red tests first:** add failing differential tests for a few F7 certificates, accepted and rejected, and for the expected type, budget, decoded-term and checked-erasure gates. Record the current function-level inventory and nonblank, noncomment line count of hand-written Clojure that decides acceptance or program behavior.
2. **Spike:** diagnose those four obstacles; add a code-generating variant of `kdef!`, or use `a/defn` when it can express the definition. Extract `encE`, `decCert` and `Check`, then run the red certificates through them. If Ansatz cannot compile a function, save a minimal reproducer and exact error in `code/lcert/test-runs/ansatz-codegen-issue.txt` for Claude to file upstream; do not post to GitHub from this task. Kernel reduction of that definition is the fallback: slow, but it adds no trust component.
3. Functionalize and prove the evaluator/erasure obligations, extract the rest of the core, route the runner through it, and turn the differential tests green. Resolve the paper/formal verdicts, establish F7 as R4's concrete encoding, and kernel-check every replacement measurement before updating the paper and tutorial. Finish with the entire lcert test suite green.
4. Record the before/after trusted-line counts with the same inventory rule, targeting **zero hand-written trusted Clojure lines** in the acceptance and execution core. Record any remaining line and why it is trusted. Write an after-action report here and a dated `LOG.md` entry with the evidence.

**Step 1 record: the trusted-line baseline (2026-10-16).** `bin/trusted-inventory`
counts, per top-level form of `src/lcert`, the nonblank lines that are not
`;`-comments (docstrings count), and classifies each form by the registry
`code/lcert/trusted-core.txt`: *trusted* if it can decide certificate
acceptance or program behavior, *shell* otherwise. Every source file must be
registered; the classification is conservative (a helper counts as trusted
unless it plainly serves only the shell; in `syntax.clj` every data operation
counts and only the parser is shell). `bin/test-trusted-inventory` checks the
rule on fixtures and runs in `bin/test-fast`. Baseline, before extraction:

| File | Trusted lines | Shell lines |
| --- | --- | --- |
| `check.clj` | 342 (21 forms) | 13 (1 form: `certificate-literal`) |
| `core.clj` | 27 (5 forms: `ns`, `type-code`, `check-code`, `check`, `run`) | 30 (7 forms) |
| `encode.clj` | 239 (30 forms) | 0 |
| `eval.clj` | 197 (14 forms) | 4 (`print-value`) |
| `kernel.clj` | 63 (11 forms) | 0 |
| `reduce.clj` | 136 (15 forms) | 0 |
| `syntax.clj` | 181 (27 forms) | 133 (11 forms: the parser) |
| `typing.clj` | 0 | 320 (23 forms) |
| `pa.clj` | 0 | 335 (33 forms) |
| `examples.clj` | 0 | 156 (22 forms) |
| **Total** | **1,185** | **991** |

The target is zero trusted lines; the after-action report reruns the same
script against the same registry rule.

**Success.** Every production acceptance and execution decision comes from extracted, kernel-checked definitions (or the documented kernel-reduction fallback); the strict runner checks caller type and `Θₙ`, executes the decoded term, and gates erasure; all existing examples and tests pass; all translated differential cases agree; each published F7 count has a kernel check; paper/formal differences are resolved or explicitly amended; and the measured hand-written trusted core reaches zero.

**Failure.** Any path executes an unchecked or different term, takes the typer's type or budget as authority, erases unchecked code, retains the old encoding for new measurements, states unproved counts, silently diverges from R4, or leaves a hand-written acceptance/evaluation decision in production. An extraction blocker must be recorded with a minimal reproducer and either a kernel-reduction fallback or an explicit unresolved failure; passing differential tests alone does not prove code generation correct.

## Trust base and consequence

The result is conditional on Ansatz's kernel and Lean's type theory as implemented there; Lean 4.29.0-rc2's `Init` is trusted **as exported** in `ansatz/init-medium.ndjson.gz` (3,653 declarations) and loaded with `:verify? false`, not rechecked by Ansatz. The ambient Init axioms found by the statement audit are `Classical.choice`, `Quot.sound` and `propext`. New formal definitions and proofs are kernel-checked. The extracted executable additionally trusts Ansatz's **unverified code generator** (as standard Coq extraction does), and Clojure and the JVM executing its output. Hand-written parsing, typing and presentation are outside the acceptance/execution trust boundary because the checked certificate, expected type, budget and decoded term gate execution. S1–S3 and other §6 hypotheses remain hypotheses of conditional theorems, not runtime axioms or claims of absolute consistency.

The abstract-machine proposal in `docs/log/2026-10-13-abstract-machine-for-lambda-cert.md` on `p2-strength` becomes **ADR-0009**, derived from the extracted evaluator and proved to preserve its behavior. It is outside ADR-0008 and is not implemented by this decision.

## Required follow-up: a formal typer (user decision, 2026-10-14)

The user's decision is to keep the typer unformalized for now, but it must eventually be formalized. That follow-up ADR defines a type-inference function in Ansatz that builds F7 certificates, proves it sound (every certificate it returns passes `Check` at the caller's type and budget), and extracts it to replace the hand-written typer. Completeness is not required for trust. Soundness would let a certificate produced by the extracted typer skip the load-time check. Until then, the typer is untrusted and every result it produces passes through `Check` as decided above.
