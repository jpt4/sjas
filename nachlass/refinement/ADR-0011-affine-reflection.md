# ADR-0011 — Affine combinators with executable self-interpretation

Date: 2026-11-27. Status: complete for the specified finite-tree machine,
including the raw-code extension and its constructive metatheory.

The current goal asks for a fresh, extremely concrete computational analogue
of self-justification. Previous TSLC/Tiler experiments did not make their
consistency principles execute. Investigate a different restriction: remove
duplication from the underlying combinator calculus, retain quotation,
evaluation, internal quotation and internal application, and include a named
type `G` isomorphic to `Box G -o Bot`.

Use finite trees and the affine basis I, K, B, C. Add E (evaluation), D
(quotation of code), M (application of code), F/U (fold/unfold at G), and a
unit value. Every contraction must strictly decrease tree node count. Types
are finite syntax with G a rigid atom; its isomorphism is explicit, never a
recursive unification equation. A separate, deliberately unsafe W extension
isolates duplication and must exhibit a well-typed loop at Bot.

Acceptance criteria:

1. Specify every type and reduction rule, including quotation of the full
   language and its evaluator. Explain precisely what Box means.
2. Give terminating executable reduction and decidable type checking on finite
   trees, with tests written before implementation.
3. Establish decrease, preservation and the normal-form argument for the
   complete calculus. Separate mathematical proofs from machine checks.
4. Construct `E : Box Bot -o Bot` and a uniform *type-indexed family*
   `Box A -o Box (A -o Bot) -o Bot`. Do not claim internal quantification
   over raw type/proof codes without actually implementing it.
5. Execute self-interpretation and code composition; adding only W must
   invalidate termination and consistency, with an exact repeating trace.
6. Audit what is missing from Willard-style self-justification and from the
   project's H0/H1 statements about its own raw-code checker.

This experiment has no agent/safety-performance claim and makes no changes
to lambda-cert or Tiler. Branch: `research/affine-reflection`. Implementation
and standalone tests live in `code/affine-reflection/`. The research record
belongs in `docs/log/` and LOG.md. A machine proof is desirable but must not be
confused with bounded enumeration or a measure assertion in the evaluator.

## After-action report

The six experiment criteria are met within the explicitly stated typed-code
scope. The complete declarative calculus has Lean proofs of strict decrease,
strong normalization, preservation, progress, consistency, H0 and H1's
type-indexed schema. Both proof files use core Lean 4.32.1 only, no classical
choice, no reflection axiom, and no `sorry`. A four-step W cycle is proved
in Lean and executed in Python; Python checks its type at every state.

The Python prototype and eleven tests are complete. A malformed-type probe
found an arity-validation bug and the accidental acceptance of `None` as a
target type; both received red/green regressions. Enumeration covers all
3,050 trees up to five nodes over ten constants, with 665 typable. This is
supplementary regression evidence, not the consistency proof.

The result makes the resource boundary exceptionally small: dropping only
W keeps full typed evaluation and removes the explicit diagonal cycle.
It does not preserve the original H0/H1 formulation over internally checked
raw codes, nor does it verify Python's unification implementation. Those
are open obligations for the broader research goal, documented in the
[research note](../../docs/log/2026-11-27-affine-reflection.md). No Tiler or
lambda-cert implementation was changed, and no agent-performance claim is
made.

## Raw-code extension (2026-11-27 continuation; completed)

The preceding goal turn made concrete progress (commit ceb9e3d2, with green
kernel and runtime evidence). The next experiment adds arbitrary raw syntax,
runtime type codes, and a primitive structural checker to the *same object
language*. Use annotated primitive instances so checking needs no unifier.
Lean's executable checker, rather than the Python prototype, will be the
authoritative algorithm for the extension.

`Box A` values store raw programs accepted at A. Add `Opposed`, whose values
store a runtime type code A and raw programs accepted at A and A -o Bot.
This is one dependent package type, representing the finite witnesses to
failure of H1; it is not a metatheoretic family of input types. H1 has the
single type `Opposed -o Bot` and executes the packaged application. Raw
checking and packaging must cover these very instructions. Neither a
blacklist of bad conclusions nor an assumed consistency theorem may be used
by the checker.

Add executable `check`, `seal A`, and `checkPair`, plus Boolean/option
eliminators so an object program can use their results. A successful seal
moves the checked code into a quotation. A successful pair check moves both
codes and the type into an opposed package. All rules must decrease expanded
term size; host checking is a structurally recursive computation, not an
oracle. Count macro-reductions separately from checker runtime.

Acceptance gates for this extension:

1. Raw constructors admit ill-typed programs and rejected W, with tests.
2. An executable checker and its primitive `check` agree by definition;
   prove preservation, progress, and consistency for acceptance by that
   actual checker, including the new instructions.
3. Prove the representation equations: quoted/packaged code is accepted
   exactly when the contained raw certificates satisfy the full checker.
   Interpret the two negative package types explicitly as H0/H1. Do not
   claim a general dependent type theory or arbitrary first-order quantifiers.
4. Execute checking of the checker's own instruction, checking/quoting the
   uniform H1 instruction, valid/invalid sealing, and an object program
   using a successfully sealed program.
5. Kernel-check termination of the implemented evaluator, and acceptance
   implies no Bot for arbitrary inputs and types, with no classical choice,
   assumed reflection, or sorry. Retain the earlier duplication counterexample.
6. Document whether the claimed raw-code representation really closes the
   gap or merely relocates it. Keep compiler/host trust explicit.

### Raw-code after-action report

All six gates pass. `RawMachine.lean` defines both the executed checker and
the checked typing relation; no external unifier or correspondence conjecture
is needed for this model. It contains a finite table of annotated primitive
types and a structural checker, including raw data, its own check opcode,
seal, uniform H1, and the dependent opposed package. The implemented
evaluator returns proof-carrying steps and is proved terminating, complete
for finding a reduction, type-preserving, and value-producing on accepted
inputs.

Kernel-checked representation equations and constructive currying identify
the negative package types with the full raw H0/H1 counterexample predicates.
The uniform H1 argument holds a runtime type code, so it is not the first
experiment's externally indexed family. The scope is these specialized
packages; a general dependent quantifier language is not claimed.

The checker is an object primitive with a fully specified algorithm, not
source code compiled into BCKI. Its runtime agreement with the checker used
in the theorem holds for every raw input and type. That is the precise sense
in which the raw-code gap is closed. The finite-tree machine meets the user's
request for a concrete executable analogue; no further implementation is
required to exhibit that model.

Behavioral examples include checking the checker, sealing H1, and executing
a checked checker to check H1. The axiom audit exposed classical choice in
one automated representation proof; explicit Boolean cases removed it. All
reported theorems now use only propext and Quot.sound. No source contains a
proof placeholder or native-decide shortcut. The old duplication-cycle
counterexample remains available.

The [scope audit and evidence](../../docs/log/2026-11-27-raw-self-justifying-machine.md)
record the whole result. The original core's historical open-obligation
paragraph above is superseded for the raw machine, not retroactively
reinterpreted as a proof about the Python checker.
