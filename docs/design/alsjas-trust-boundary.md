# ALSJAS Trust Boundary

Status: Normative for ADR-0002

Date: 2026-08-28

## Trusted for the mechanized claims

- the pinned Lean 4.32.1 kernel;
- the small Lean compiler/runtime path needed to execute the generated CLI;
- the operating system and hardware only for claims about a particular CLI
  execution; and
- human review that the formal theorem statements express the intended ALSJAS
  claims.

Lean's kernel is the external metatheory. Therefore ALSJAS consistency is
relative to that metatheory; the project does not claim absolute foundational
consistency.

## Not trusted for native acceptance

- Lean tactics or proof search at CLI runtime;
- host pointer identity, hash equality, memo tables, or DAG sharing;
- the surface parser without rechecking the resulting AST;
- precomputed demo results;
- a theorem name, comment, test name, or constant called `self0`;
- bounded proof search or failure to discover a contradiction; and
- the universal-machine driver as a source of proof evidence.

The native checker accepts only a supplied structural system identity and a
complete structural proof or trace tree. Lean proves that this executable
decision procedure corresponds to the inductive relation.

## Forbidden project mechanisms

The ALSJAS source tree must contain no:

- `sorry` or `admit`;
- `unsafe` definitions;
- project-local `axiom` declarations;
- `implemented_by` or foreign replacement of verified decisions;
- host oracle that returns native derivability; or
- import of numeric program-code machinery into core reflection results.

The audit allows Lean's documented foundational axioms only when they are
introduced by the standard library and are explicitly listed by
`#print axioms`. The target is for all headline propositions to depend only on
Lean's ordinary logical foundation; any additional dependency is a failed
gate until reviewed and recorded.

## Executable boundary

The CLI is evidence that concrete certificates are inspectable and that
negative controls are rejected. It is not evidence of consistency. The
consistency, preservation, G2, and non-definability claims are supported only
by checked Lean theorems plus their axiom audit.

The machine driver may be fuel-bounded and may fail to terminate only through
an explicitly separate partial interface. No such computation can be coerced
to a proof term.
