# Affine reflection experiment

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
The object language has typed quotations, not an internal raw-code checker.
H1 is a type-indexed schema, not internal quantification over type codes.

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
