# ADR-0003: Put the ALSJAS theory paper before further implementation

Status: Accepted for sequencing; theory review in progress

Date: 2026-08-29

Branch: `adr-0003-alsjas-paper-first`

Theory paper: [`../theory/alsjas-calculus.md`](../theory/alsjas-calculus.md)

Prior decision: [`ADR-0002-alsjas.md`](ADR-0002-alsjas.md)

## Context

ADR-0002 correctly made the headline metatheorems completion gates, but it put
executable implementation before a complete paper definition of the object
calculus. The companion design listed types and term constructors and sketched
proof ideas. It did not define the modal base, `self0` extension, and copy
control as distinct formal rule sets; give all typing and reduction rules; or
supply natural-language proofs detailed enough to anchor the Lean statements.

Implementation through the first reflective artifacts exposed the cost of
that reversal. Checker adequacy had to invent an intrinsic relation after the
checker existed. The executable reduction treated boxed composition and
positive introspection differently from the proof argument assumed by the
design. The abstract G2 interface expressed binders as unrestricted host
functions, so affinity was a convention of one term rather than a property of
the interface. Finally, the “polymorphic copy” theorem quantified over a
polymorphism that the grammar did not contain.

These are specification findings, not isolated coding defects. Continuing to
patch the implementation would allow the program to choose the theory.

## Decision

Suspend additional ALSJAS implementation and make the paper
[`alsjas-calculus.md`](../theory/alsjas-calculus.md) a prerequisite for all
remaining code work.

The paper must define and justify at least:

1. the complete type, term, context, and finite-derivation grammar;
2. the three profiles `B[S]`, `J[S]`, and `C[S]`;
3. affine context splitting, weakening, and the absence of contraction;
4. closed proof-carrying quotation without general unboxing;
5. boxed composition, positive introspection, and the selected fixed point;
6. substitution, reduction, subject reduction, normalization, and canonical
   forms;
7. base consistency and structural elimination of `self0` at contradiction;
8. the explicit box-contraction-dependent computational G2 derivation;
9. open-type and proof-provenance forms of copy non-definability;
10. checker soundness/completeness and a structural recursion bound;
11. exact baseline/control identity separation; and
12. the boundary separating these results from Turing representability.

The paper is a pre-mechanization specification, not proof evidence. Its
natural-language arguments determine the formal theorem statements and proof
decompositions; Lean must still check every accepted theorem.

## Review gate

Implementation remains suspended until a review explicitly resolves the six
open questions in Section 19 of the paper and records one of these outcomes:

- **Accepted:** the grammar and paper proofs are the mechanization target;
- **Revise:** named sections require correction before code resumes; or
- **Abandon:** the intended self-justification or matched-control claim cannot
  be supported by this calculus.

Acceptance is not inferred from silence, successful compilation, existing
demos, or agreement between the paper and current code.

After acceptance, mechanization resumes in the theorem-led order in Section 17
of the paper. Each slice begins with a failing theorem probe, but no executable
feature may get ahead of the paper theorem on which its meaning depends.

## Treatment of existing implementation

Existing commits on `adr-0002-alsjas` are preserved as an exploratory
prototype. They are not retroactively declared conforming. Before any of that
code is merged, every module must be mapped to the paper and classified:

- conforms unchanged;
- requires revision;
- exploratory but outside the trusted calculus; or
- must be removed.

The interrupted checker-adequacy slice remains uncommitted in its original
worktree. The paper-first branch was created as a separate worktree from the
last green commit so unfinished code cannot enter this decision accidentally.

## Required documentation changes

- ADR-0002 records that its implementation order is suspended by this ADR.
- The formal design points to the paper as the normative calculus.
- The theorem dependency graph begins with paper acceptance and the intrinsic
  grammar, not S-expression code.
- The acceptance matrix gains paper and calculus-conformance gates.
- Future After Action Reports compare implementation to the paper rather than
  treating current behavior as the baseline.

## Alternatives rejected

### Continue checker adequacy and write the paper later

Rejected. Adequacy can only relate a checker to an independently defined
relation. Defining the relation around existing checker branches would be a
tautological validation of the implementation rather than a theory result.

### Treat `docs/design/alsjas.md` as the paper

Rejected. It is an architecture and theorem roadmap. Its constructor sketches
omit formal judgments, several reductions, profile separation, and complete
proof arguments.

### Freeze the current code as the formal specification

Rejected. The research claim concerns a mathematical calculus and its
metatheory, not a particular sequence of Lean definitions. Code remains
evidence only after refinement and adequacy are proved against the paper.

### Discard all existing work immediately

Rejected. The prototype contains useful parsers, tests, negative controls, and
explicit candidate proof programs. Preserving it makes the eventual
conformance audit concrete without allowing it to govern the theory.

## Consequences

- No additional Lean, CLI, machine, or reflection-demo implementation occurs
  before paper review.
- Some existing definitions will likely change, especially profile indexing,
  box administrative reduction, checker fuel, and the abstract G2 interface.
- The paper may falsify or narrow a desired claim. That is a successful use of
  the gate, not a reason to weaken it after implementation.
- Red-green TDD remains mandatory after the theory gate, but theorem statements
  now precede executable tests in the dependency order.
- ADR-0002 remains the research objective and scope record; this ADR corrects
  its sequencing and supplies the missing theory layer.
