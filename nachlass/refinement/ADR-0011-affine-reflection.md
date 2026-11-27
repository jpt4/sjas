# ADR-0011 — Affine combinators with executable self-interpretation

Date: 2026-11-27. Status: typed-core experiment complete; raw-checker extension open.

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
