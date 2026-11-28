# Affine reflection: a concrete self-justifying machine

The current model is the **raw-code machine**, with its own executable
checker, raw program/type inputs, H0, and one uniform H1 over runtime type
codes. The checker and evaluator are the same Lean definitions used by the
whole-system proofs. Read the
[construction and completion audit](../../docs/log/2026-11-27-raw-self-justifying-machine.md).

From this directory, using the installed Lean 4.32.1 toolchain:

```sh
LEAN_PATH=. ~/.elan/bin/lean +leanprover/lean4:v4.32.1 -o RawMachine.olean RawMachine.lean
LEAN_PATH=. ~/.elan/bin/lean +leanprover/lean4:v4.32.1 -o RawChecks.olean RawChecks.lean
LEAN_PATH=. ~/.elan/bin/lean +leanprover/lean4:v4.32.1 --run RawDemo.lean
```

The demo checks the checker's own instruction, seals H1, runs a checked
checker to check H1, uses a sealed program, and rejects a purported Bot
certificate. Kernel theorems cover arbitrary inputs, not just these demos:
checker H0/H1, exact representation of certificate packages, preservation,
progress, and termination/correctness of the implemented evaluator.

The checker is a primitive structural algorithm. H0/H1 are encoded by
negating finite counterexample packages; arbitrary dependent quantifier
syntax and source-level self-hosting of the checker are outside this model.
The axiom audit uses only propext and Quot.sound. See
[`validation-raw.txt`](validation-raw.txt) for the complete validation run.

## Original minimal core

A concrete combinator calculus with executable self-interpretation and a
four-step inconsistency loop when duplication is added. The complete design,
proofs, and limitations are in the
[research note](../../docs/log/2026-11-27-affine-reflection.md).

Run with Python 3.12 (standard library only):

```sh
python3 machine.py
python3 -m unittest -v test_machine
```

Check the declarative metatheory with Lean 4.32.1, core only:

```sh
lean +leanprover/lean4:v4.32.1 -o Decrease.olean Decrease.lean
LEAN_PATH=. lean +leanprover/lean4:v4.32.1 Soundness.lean
```

`Decrease.lean` proves strict size descent and strong normalization for every
finite term, and an infinite run after adding W. `Soundness.lean` proves
preservation, progress, consistency, and the typed H0/H1 combinators for the
whole declarative language. The printed axiom lists contain only `propext`
and `Quot.sound`; H1 needs neither. There is no `sorry` or classical choice.

`machine.py` implements finite syntax, an external unification-based type
checker, rewriting, and affine bracket abstraction. Python inference has
tests but **no verified refinement theorem** to Lean's declarative relation.
That original object language has typed quotations, not an internal raw-code
checker. Its H1 is a type-indexed schema. The raw-code extension above closes
these two gaps using an annotated checker and exact dependent packages; it
does not rely on the original Python inference implementation.

The tests cover all eight safe contraction rules, all primitive typing
schemes, composition and self-interpretation, both typed consistency
combinators, malformed types, rejection of W even under quotation, the
unsafe four-step cycle, and all 3,050 trees of at most five nodes over the
ten-constant alphabet. Exactly 665 of those trees are typable. Enumeration
is a regression check; the all-size consistency theorem is in Lean.

Coverage is not total: defensive failures for malformed Python-host objects,
some abstraction-helper paths, and the printable demo are not all exercised
by unittest. The separate demo command exercises the printing path. These
are documented implementation gaps, not assumptions of the formal theorem.
