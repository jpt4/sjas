# ADR-0005 — λᶜᵉʳᵗ as a minimal programming language, on Clojure and Ansatz

**Status.** Accepted 2026-09-25. Implementation in progress.

**Branch.** `adr-0005-lcert`, branched from `sjas-codification`, and merged
back into it when complete. The R4 work has not yet reached `master`, so this
branch's parent is the branch that carries R4.

**Depends on.**
- [`R4-certificate-calculus.md`](R4-certificate-calculus.md), the design;
- [`R4-metatheory.md`](R4-metatheory.md), the core calculus λᶜᵉʳᵗ₀, its rule
  table, encoding properties E1–E5, and the budgeted evaluator of §5.

## Context

The goal set on 2026-09-25 ends: "implement the calculus as a minimal
programming language using the Clojure Ansatz library" (`../LOG.md`, that
date).

**What Ansatz is.** Ansatz (Maven coordinates org.replikativ/ansatz, 0.2.115) is a
dependently typed Clojure DSL on a Lean 4-compatible kernel. Its verified
definitions compile to ordinary Clojure functions.

**What the spike found** (`../LOG.md`, 2026-09-25, steps 1–3):
- *Works:* `Nat`- and `Bool`-valued functions of a user-defined tree type are
  defined, compiled and provable, offline, in one JVM of about 420 MB.
- *Not supported:* leaf constructors with fields.
- *Not generated:* equation lemmas for functions that return trees.

**The machine** has little free memory and little free disk. At most one JVM
may run at a time.

## Decision

**1. The language.** A minimal implementation of λᶜᵉʳᵗ₀ in Clojure, under the
new directory code/lcert of this repository. Its components:
- **Surface syntax** in EDN, with named variables, compiled to de Bruijn
  terms.
- **A type checker** with usage accounting, following the metatheory's rule
  table (§1.4). It builds an **explicit derivation** for every judgment,
  including conversion chains.
- **An encoding** of derivations into codes. It extends the table of the
  draft's §2.7 and satisfies E1–E5. The draft's `not` example, encoded by it,
  has exactly 35 internal nodes.
- **`Check`**, a structural validator of encoded derivations. It is
  independent of the type checker: it re-checks each rule instance locally, and
  calls itself on the smaller codes of δ-steps.
- **The budgeted evaluator** `evalₙ` of the metatheory's §5:
  - call-by-value and non-erasing;
  - runtime tokens as distinct objects;
  - `reflect` capped by the certificate's footprint;
  - `inspect` rebinding the certificate it checks.
- **A small API:** check a program at a budget, certify a closed program,
  build its certificate from tokens, and run.

**2. The Ansatz part — a verified kernel.**
- In Ansatz: the code tree type, the node count, and the root context length
  (the budget) as verified functions.
- Proved by the kernel:
  - the strict-overhead inequality (metatheory Lemma 2.7);
  - the budget arithmetic of the `H₁` case;
  - the counts of the draft's §2.7 example, by computation.
- The compiled functions replace the plain-Clojure measures at runtime when the
  kernel is loaded. The two are differentially tested for agreement.

**3. Two test suites.**
- A fast suite, which needs no Ansatz.
- An extended suite, which loads the kernel. It re-runs the language tests on
  the verified measures, and runs the differential and theorem tests.

## Alternatives considered

- **The whole checker as Ansatz definitions,** with a proof that it is sound.
  - *Its merit:* the strongest guarantee.
  - *Why not:* the checker returns and compares trees throughout, and the spike
    found that theorems about tree-returning functions do not go through in
    this Ansatz version. It would also be weeks of work on a young DSL.
  - *Status:* recorded as a follow-up, not rejected on principle.
- **A shallow embedding of λᶜᵉʳᵗ into Ansatz's kernel.**
  - *Why not:* the kernel is not affine and has no usages, so the token
    discipline — the point of the calculus — would be lost.
- **No Ansatz at all.** Rejected: the goal names it. Its kernel is also the
  right place for the one inequality the consistency proof turns on.

## Success criteria

1. **Faithfulness.**
   - Every rule of the metatheory's §1.4 is implemented in the type checker and
     in `Check`.
   - The draft's `not` example encodes to 35 internal nodes. `Check` accepts it
     at `Bool → Bool`, and rejects it at any other type.
2. **Round trip.**
   - Every derivation the type checker builds is accepted by `Check` at its
     type.
   - Every derivation mutated by a single-node change is rejected, unless the
     mutation produces another valid derivation.
3. **Resource discipline, tested.** The type checker rejects each of these:
   - a token used twice;
   - a token captured by an eliminator method;
   - a certificate node built without a token;
   - a closed term of type `R` other than a leaf.

   At runtime, no token object ever occurs twice in a value.
4. **The self-reference constants.**
   - `H` and `H₁` type as the closed inhabitants of `H°` and `H₁°`.
   - `inspect` makes a certificate survive its own check.
   - `reflect` at `Nat` runs a certified program with fewer tokens than its
     certificate holds, and returns its value.
   - D1: `(lit, ⋆) : □(Bool → Bool)` checks at budget 35.
5. **Ansatz.**
   - The kernel accepts the definitions and the theorems, offline, with no
     Mathlib store.
   - Its compiled measures agree with the plain ones on every code in the test
     corpus, and on generated random codes.
6. **Hygiene.** Tests precede code, red then green. Code is commented for a
   reader new to the project. A README documents the language.

## Failure criteria

Any one of these fails the ADR, and is recorded in its after-action report
rather than hidden:
- a program the type checker accepts at type `0` in any budget;
- `Check` accepting a code that decodes to no valid derivation;
- a token duplicated at runtime by a well-typed program;
- the Ansatz kernel refusing the strict-overhead theorem;
- the verified and plain measures disagreeing.

The first three would contradict the metatheory. They would be reported as
possible defects of the proofs, not only of the code.

## Consequences

- The metatheory gains a mechanized component in its §7.
- The draft's §2.7 numbers become test oracles.
- The language is a vehicle for experiments the draft proposes, for example a
  relative encoding (draft §7).
