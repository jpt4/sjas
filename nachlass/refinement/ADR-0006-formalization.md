# ADR-0006 — Formalizing the R4 metatheory in Ansatz

**Status.** Accepted 2026-09-27. In progress.

**Branch.** `adr-0006-formalization`, from `sjas-codification`, merged back
into it phase by phase.

**Depends on.**
- [`R4-metatheory.md`](R4-metatheory.md), the metatheory to be formalized;
- [ADR-0005](ADR-0005-lcert-implementation.md), whose Ansatz kernel
  (`code/lcert/ansatz/lcert/verified.clj`) this extends.

## Context

The goal set on 2026-09-27: "Formalize both the statements and the proofs of
the metatheory, using the Ansatz Lean-in-Clojure library."

So far Ansatz checks six small facts (ADR-0005). Everything else in
`R4-metatheory.md` is a paper proof, reviewed six times.

**What the spikes of 2026-09-27 found** (`../LOG.md`, that date):
- *Works:* inductive types, indexed inductive families in `Prop`, the tactics
  `apply`, `exact`, `induction`, `cases`, `omega`, `rfl` and others, and
  large elimination. The set-theoretic model of §3 needs large elimination: a
  `Type`-valued function on skeletons.
- *Needs a workaround:*
  - `a/defn` cannot define a `Type`-valued or dependently typed function. It
    mis-infers the recursor's universe, and its Clojure code generator cannot
    compile a type.
  - The workaround: elaborate the term with Ansatz's surface elaborator, name
    the recursor's universe explicitly (`Sk.rec.{2}`), and install the
    definition with the kernel's `check-constant`, bypassing code generation.
    This checks the definition with the kernel, like any other.
- *Surface details:* the arrow is `=>`. Inductive predicates need `:in Prop`.
  The Clojure reader needs universe levels written as `Sk.rec$2`, rewritten
  after reading.

## Decision

**1. What "formalized" means here.** Every numbered definition, lemma,
proposition and theorem of `R4-metatheory.md` gets a kernel-checked
counterpart: a statement, and a proof with no `sorry` and no axioms beyond
those listed in 2.
- A result is *formalized* when its statement and proof are accepted by the
  Ansatz kernel.
- A result is *stated* when only its statement is, with the proof pending,
  which this document then records.

**2. The trust base.** The only assumptions are these, each an explicit
hypothesis of the theorems that use it, never a global axiom:
- **The checker's specification** (`CheckSpec`). The metatheory uses `Check`
  only through Lemmas 2.6–2.8 and the encoding properties E1–E6. The model's
  theorems therefore quantify over any `chk` that meets those properties.
  Constructing the concrete `Check` in Ansatz and proving it meets them is
  phase F7.
- **The three cited results of §6** (S1–S3), for P5 only. Gödel's second
  theorem for PA, PA's Σ₁-completeness, and E-PA^ω's conservativity are not
  proved here. They enter as hypotheses of the P5 theorem.
- Lean's `Init` environment, as bundled with Ansatz.

**3. The design.**
- *A deep embedding:* one inductive type `Exp` for types and terms, as the
  encoding already treats them, with de Bruijn indices; contexts as lists of
  usage–type pairs.
- *The typing judgment:* an inductive predicate `Der chk σ Γ t A`,
  parameterized by the checker used for δ-steps; conversion as an inductive
  predicate over single recorded steps.
- *The model:* carriers `Car : Sk → Type` by large elimination. Denotation is a
  relation `Den n η t v`, since a `Prop` judgment cannot be eliminated into
  `Type`. `V` is defined by recursion on types into `Prop`.

**4. The phases,** each a namespace under `code/lcert/formal/`, loaded by a
new `bin/test-formal`:

| Phase | Content | Metatheory |
| --- | --- | --- |
| F0 | the `kdef` helper, conventions, the suite | — |
| F1 | usages and their laws; `Exp`, contexts, lifting and substitution | §1.1–1.3 |
| F2 | the rule table, conversion, skeletons; the syntactic lemmas | §1.4–1.5, §2 |
| F3 | carriers, defaults, denotation, `V`, environments; the fundamental lemma; T1, Corollary 3.7, T2, T3 | §3 |
| F4 | §4: Propositions 4.1–4.12, Theorem 4.6, Proposition 4.10′ | §4 |
| F5 | the evaluators; T4, T4′, Theorem 5.2 | §5 |
| F6 | P5, relative to S1–S3 | §6 |
| F7 | the concrete encoding and `Check`, and the proof of `CheckSpec` | §1.6 |

**5. Testing.** A formal statement is its own test: the kernel rejects it
until it is proved, which is the red step. Each namespace also carries
negative checks, such as a false variant the kernel must reject, so that the
statements are not vacuous.

## Success and failure

- **Success:** F1–F6 formalized in the sense of 1, with the trust base of 2,
  and F7 formalized, or recorded as the one open obligation with the reason.
- **Failure:** a result that cannot be formalized because it is false. The
  metatheory is then corrected and the correction recorded. Or a result that
  cannot be formalized because of Ansatz. That is recorded, with the
  limitation, as a stated-only result.
- **A review question for each phase:** does the formal statement say what the
  paper says? Differences are recorded here.

## Deviations recorded during the work

Each is a difference between design 3 above, or the paper, and what the kernel
now checks.
- **Denotation is a function, not a relation.** `den n t G s η : Car s` is
  defined as a table of levels, `denT n n`. `T (k+1) m` reuses row `k` below
  level `m` and re-evaluates at `m`. Reflect runs its decoded program at
  exactly `⟦t′⟧ᵐ` (`denT_stable`). Typing is split into `Tl` (formation) and
  `Rt` (runtime, with usage vectors), with skeleton typing `SkJ` beside them.
- **Branch lists are a constructor, `tBrs P k`,** with skeleton `Lbl → skel P`.
  Ansatz's inductive compiler cannot derive recursors for function-typed
  fields. The first skeleton given to `tBrs` broke Lemma 2.5, which Codex
  found; the present one does not.
- **`SkJ`'s reflect rule requires a base type.** Grok found that Lemma 3.1
  fails for reflect at an open type. The paper's rule has the same premise
  implicitly, since `chk′`'s type is closed. The formal rule states it.
- **`V` takes the skeleton explicitly,** since carriers depend on it.

## Consequences

- The metatheory's status line will change from "paper proofs" to naming what
  the kernel has checked.
- The formal suite is slow, since Ansatz elaborates every declaration at load.
  It is kept separate from the fast and extended suites.
