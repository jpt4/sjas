# ADR-0002: Affine Lambda SJAS

Status: Accepted research scope; implementation sequence suspended by
[ADR-0003](ADR-0003-alsjas-paper-first.md) pending theory-paper review; every
headline metatheorem remains a completion gate

Date: 2026-08-28

Sequencing revised: 2026-08-29 by ADR-0003

Branch: `adr-0002-alsjas`

Companion design: [`../design/alsjas.md`](../design/alsjas.md)

Normative calculus draft:
[`../theory/alsjas-calculus.md`](../theory/alsjas-calculus.md)

Theorem dependency graph:
[`../design/alsjas-theorem-dependencies.md`](../design/alsjas-theorem-dependencies.md)

Trust boundary:
[`../design/alsjas-trust-boundary.md`](../design/alsjas-trust-boundary.md)

Acceptance matrix:
[`../testing/alsjas-acceptance-matrix.md`](../testing/alsjas-acceptance-matrix.md)

Planned implementation root: [`../../code/alsjas/`](../../code/alsjas/)

## Context

ADR-0001 opened a purpose-built affine-tree kernel as one answer to the
question whether an SJAS can manipulate its own syntax without using numbers
as its internal carrier. The accepted ALSJAS plan asks a distinct and stronger
question: can a native computational proof language both establish a
coding-independent computational version of G2 and demonstrate, by a matched
one-rule control, exactly which resource permits that theorem to fire?

Merely adding a constant called `SelfCons` would not answer the question. A
candidate must prove that its native self-consistency program preserves
consistency, prove a no-go theorem for a closely matched stronger calculus,
and establish that the missing operation is semantically non-definable rather
than just absent from the grammar.

The plan also asks for Turing-complete *representability*. That phrase is
deliberately weaker than a total universal evaluator. Arbitrary partial
computations must have native descriptions and every finite run must be
checkable; termination of all runs and general recursion in proof terms remain
out of scope.

## Decision

Create a separate research line named **Affine Lambda SJAS (ALSJAS)** under
`code/alsjas`. It does not supersede ADR-0001, change `code/tabt`, or reuse the
unrestricted `lambda-sjas` prototype as its trusted object calculus.

ALSJAS is an intuitionistic affine modal lambda calculus with S-expression
surface syntax and the following defining properties.

1. Contexts admit exchange and weakening but not contraction. The core types
   are `0`, `1`, `tensor`, `lolli`, and `box S A`.
2. A boxed value contains the complete native proof tree accepted by the exact
   structural system identity `S`. It is opaque to object programs and is
   consumed linearly.
3. Closed quotation, boxed composition, and positive introspection are
   available. General unboxing, a proof-box recursor, DAG sharing, a total
   evaluator, and polymorphic copying are not.
4. Each system is a canonical, finite `(mu self ...)` S-expression. The binder
   may occur only in designated modal identity positions. Changing a rule,
   axiom, or checker profile changes the identity and regenerates reflective
   artifacts.
5. The system contains one selected Goedel-Jeroslow fixed point, represented
   natively, but exposes no uniform fixed-point generator.
6. The distinguished Level-0 consistency program has type
   `self0 : box S 0 lolli 0`. A native `Clash1 S` package supports the derived
   Level-1 interface `self1 : Clash1 S lolli 0`.
7. `ALSJAS+Copy` differs by exactly one capability:
   `copy-box : box S A lolli tensor (box S A) (box S A)`. It has a regenerated
   identity and regenerated reflective artifacts.
8. Lean 4.32.1 is the external mechanization and executable host. Native
   derivability is decided by the verified structural checker, never by asking
   Lean to prove an object-language proposition.
9. The Turing bridge is a separate dependency layer over native program,
   configuration, transition, and trace trees. Core reflection and consistency
   results may not import that layer or arithmetic program-code facilities.

## Required theorems

The project is incomplete until all of the following are proved without
`sorry`, `admit`, `unsafe`, or project-local axioms.

### Core metatheory

- S-expression parser and printer round trips on canonical inputs.
- Decidability and adequacy of scope, affinity, typing, proof checking, and
  structural identity checking.
- Deterministic reduction, subject reduction, normalization, and canonical
  forms for the proof-term calculus.
- Preservation of explicit tree payloads and impossibility of manufacturing
  hidden sharing or duplicating an unknown box during reduction.

### Computational G2

- A coding-independent theorem over an abstract reflective proof-program
  calculus with a Goedel-Jeroslow fixed point, necessitation, boxed
  composition, positive introspection, and boxed contraction.
- A complete derivation/proof term showing that a consistent instance cannot
  contain a closed inhabitant of `box S 0 lolli 0`.
- A dependency audit showing that the theorem uses no numeric syntax code.

### Self-justification

- Consistency of the minimal reflective base by normalization and canonical
  forms.
- The relative preservation theorem
  `Deriv (Base + self0) 0 -> Deriv Base 0`.
- A structural proof based on strict constructor descent through quoted proof
  payloads, not a numeric proof-code bound.
- Instantiation for the exact sealed ALSJAS identity, with checker-accepted
  `self0`, `collapse`, and `self1` artifacts.

### Matched copy control

- `copy-box` realizes precisely box-restricted contraction.
- Computational G2 instantiates for `ALSJAS+Copy` and yields an explicit,
  checker-accepted closed `boom : 0`.
- The same raw term is rejected by baseline ALSJAS.
- No polymorphic baseline term defines `copy-box`, proved using a resource
  logical relation or a free affine model.
- Other possible diagonal/copy routes are enumerated and discharged; the
  proof may not appeal only to the slogan that the calculus is affine.

### Turing representability

- A native structural machine model, one-step relation, and finite-trace
  checker.
- Soundness and completeness of the executable trace checker.
- A structural correspondence with a standard Turing-complete machine model
  and representation of arbitrary partial computations.
- Verification of every supplied finite run, with no total universal `run` and
  no theorem that all encoded runs halt.

## Paper-first and test-first implementation order

ADR-0003 supersedes the ordering below. Before step 1, the calculus grammar,
judgments, reductions, and natural-language proofs in the normative theory
paper must be reviewed and accepted. Existing implementation is exploratory
until it passes the resulting conformance audit.

Every executable feature begins with a failing test or compile-time theorem
probe. The sequence is:

1. canonical S-expressions and parser/printer;
2. types, contexts, raw terms, affinity, and typing;
3. structural system identities and stale-identity rejection;
4. proof trees, boxes, checker, and reduction;
5. metatheory and axiom-dependency audit;
6. abstract computational G2;
7. `self0` preservation and concrete reflective artifacts;
8. `ALSJAS+Copy`, `boom`, baseline rejection, and non-definability;
9. isolated machine/trace layer;
10. CLI integration and full negative-control suite.

Red outputs are retained under `code/alsjas/test-runs/`; a green test does not
discharge a theorem unless the corresponding Lean declaration is checked and
its axiom dependencies are printed.

## Alternatives rejected

### Extend `lambda-sjas`

Rejected. Its unrestricted beta substitution duplicates terms and it is not
governed by the ALSJAS theorem gates. It remains untouched as a separate
prototype.

### Treat ADR-0001 as the implementation specification

Rejected. ADR-0001 governs the first-order affine-tree `TabT` family and its
Ruby kernel. ALSJAS is a modal Curry-Howard experiment with a Lean
mechanization and a matched contraction control. The projects may later share
lessons, but neither silently replaces the other.

### Omit `copy-box` from the syntax and infer non-definability

Rejected. Missing syntax is weaker than missing capability. The baseline must
have a semantic non-definability proof, and the extension must demonstrate the
actual contradiction.

### Claim Turing completeness from a host evaluator

Rejected. An opaque or total host `run` would neither establish native
representability nor preserve the proof/computation boundary. The bridge uses
native descriptions, steps, and finite traces in a separately imported layer.

### Infer consistency from execution

Rejected. Tests, bounded searches, successful normalization examples, and the
absence of a discovered contradiction are not consistency evidence. Only the
mechanized preservation and canonical-forms results satisfy the gate.

## Consequences

- This ADR deliberately creates a large proof burden. A useful CLI without the
  headline metatheorems is an incomplete research artifact, not completion.
- The system identity includes its rules, axioms, modal profile, and checker
  version. The baseline and copy control can never share a reflective identity.
- The core dependency graph remains free of arithmetic coding. External Lean
  implementation details may use ordinary finite-data indexes, but no
  arithmetic encoding theorem may occur on a dependency path to the headline
  reflection results.
- The partial-computation layer can represent nontermination without allowing
  divergence to inhabit proof types.
- Any failed theorem triggers a design revision and an ADR/AAR update; it is
  not patched by weakening the stated completion criterion.

## Primary references

- Lev D. Beklemishev and Daniyar S. Shamkanov, “Some Abstract Versions of
  Goedel's Second Incompleteness Theorem Based on Non-Classical Logics,” 2016,
  especially Section 3, Theorem 3, Remark 3.9, and Section 4.
  <https://arxiv.org/abs/1602.05728>
- Frank Pfenning and Rowan Davies, “A Judgmental Reconstruction of Modal
  Logic,” 2001. <https://www.cs.cmu.edu/~fp/papers/mscs00.pdf>
- P. N. Benton, *A Mixed Linear and Non-Linear Logic: Proofs, Terms and
  Models*, 1994.
  <https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-352.html>
- David Michael Roberts, “Substructural Fixed-Point Theorems and the Diagonal
  Argument: Theme and Variations,” 2023.
  <https://doi.org/10.32408/compositionality-5-8>
