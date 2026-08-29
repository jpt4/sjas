# ALSJAS Formal Design

Status: Architecture for ADR-0002; implementation suspended by ADR-0003
pending review of the normative calculus paper

Date: 2026-08-28

Paper-first revision: 2026-08-29

Normative object calculus:
[`../theory/alsjas-calculus.md`](../theory/alsjas-calculus.md)

Sequencing correction:
[`../adr/ADR-0003-alsjas-paper-first.md`](../adr/ADR-0003-alsjas-paper-first.md)

The grammar and proof sketches below are architectural summaries. Where they
differ from the paper, the paper governs. They must not be used independently
to invent Lean rules.

## 1. Purpose

ALSJAS is designed to make one comparison exact:

```text
baseline native reflective calculus without definable box contraction
  + self0
    -> structurally consistency-preserving

the same calculus with copy-box and a regenerated identity
  + self0
    -> explicit contradiction by computational G2.
```

The implementation must make syntax, derivations, proof payloads, system
identity, and finite computation traces inspectable native trees. Numeric
Goedel codes play no role in the object calculus or in the dependency paths of
the reflection theorems.

## 2. Layers

The Lean library is split so import direction enforces the intended boundary.

```text
SExpr
  -> Surface
  -> Core.Syntax
  -> Core.Context / Core.Typing
  -> Core.Reduction / Core.Checker
  -> Core.Metatheory
  -> Reflection.AbstractG2
  -> Reflection.SelfCons
  -> Control.Copy

SExpr -> Machine.Syntax -> Machine.Trace -> Machine.Representability

CLI imports both branches; no Core/Reflection/Control module imports Machine.
```

`AxiomAudit` imports every headline theorem and prints its Lean axiom
dependencies. A separate source audit rejects forbidden declarations and
imports.

## 3. Native S-expressions

The shared carrier is:

```text
SExpr ::= atom UTF8-atom | list (List SExpr)
```

The concrete grammar supports parentheses, ASCII atoms, quoted strings with a
small explicit escape set, and comments only at the host source level. The
canonical printer emits one representation; parser/printer round-trip theorems
are stated over canonical ASTs, while tests also cover whitespace and malformed
input.

No pointer identity, hash, or host object address is semantically relevant.
Equality is structural. Box payloads, proof trees, and machine traces serialize
as ordinary S-expressions and are reparsed before public checker acceptance.

## 4. Types and system references

Core types are:

```text
Ty ::= zero
     | one
     | tensor Ty Ty
     | lolli Ty Ty
     | box SystemRef Ty
     | godel SystemRef
     | atom AtomName
```

`atom` supports protected contract atoms and the universal quantification used
by the copy non-definability theorem. It does not create an elimination rule.

A `SystemRef` is either a closed canonical system S-expression or the bound
marker `self`. The marker is legal only beneath the unique outer
`(mu self BODY)` system binder and only in modal system-reference fields. The
sealing pass rejects all other occurrences and substitutes the entire sealed
tree structurally. It does not compute or store a numeric digest.

The selected fixed point is the recursive proposition

```text
G_S ~= box S (lolli G_S zero).
```

It is exposed through the two typed constructors `godelFold` and
`godelUnfold`. There is no syntax taking an arbitrary type context and
returning its fixed point.

## 5. Affine terms and contexts

Raw terms use de Bruijn variables:

```text
Term ::= var Nat
       | triv
       | pair Term Term
       | letTensor Term Term
       | lam Ty Term
       | app Term Term
       | abort Ty Term
       | quote SystemRef ProofTree
       | boxComp Term Term
       | boxIntrosp Term
       | godelFold Term
       | godelUnfold Term
       | self0 SystemRef
       | collapse1 SystemRef
       | copyBox SystemRef Ty       -- control profile only
```

The parser may carry names, but elaboration resolves them to de Bruijn
indices. A typing judgment returns a usage bit-vector as well as a type. Each
input assumption may be used zero or one times; sibling premises must have
disjoint usage. Weakening is therefore implicit, contraction impossible.

`quote S p` is accepted only when `p` is a complete closed proof tree for `A`
under the exact `S`; its result has type `box S A`. There is no eliminator that
turns a box into its conclusion. `boxComp` and `boxIntrosp` build new explicit
proof trees by structural constructors while consuming each input once.

The `copyBox` constructor is parsed only under the `with-copy` rule profile.
Its public term has type:

```text
box S A lolli tensor (box S A) (box S A).
```

The implementation of the control contains two separately serialized copies
of the input payload. A shared host pointer is never an accepted certificate.

## 6. System identity

A system specification contains, in canonical order:

- format and checker versions;
- the affine structural profile;
- the modal rules;
- the selected fixed-point declaration;
- the distinguished `self0` and Level-1 declarations;
- optional user contract declarations; and
- the negative-control capability flag.

The source form is `(mu self SPEC)`. `seal` validates the restricted uses of
`self` and returns a finite recursive identity object represented by the source
tree plus a binding discipline. Reflective comparison uses alpha-invariant
structural equality of sealed objects. There is no attempt to expand `self`
into an infinite tree.

Every accepted proof tree carries the canonical sealed system source.
`checkProof expected proof` first compares identities and then checks every
node. A proof from `ALSJAS+Copy`, a proof from a stale baseline version, and a
proof with an edited rule list all fail before their inference nodes are used.

## 7. Proof trees and checker

`ProofTree` is an untrusted AST with a conclusion type, system identity, and
one constructor for each rule. The executable checker recursively returns a
checked conclusion and usage certificate or a structured error. It does not
invoke Lean theorem search.

The intrinsic relation `Deriv S Gamma A` mirrors successful checks. The
adequacy interface has both directions:

```text
checkProof S raw = ok A  -> Deriv S [] A
Deriv S [] A             -> exists raw, checkProof S raw = ok A
```

The first direction is the security-critical soundness theorem. The second
ensures the raw certificate language is not accidentally weaker than the
formal calculus.

All payloads are trees. The checker rejects references, back-edges, node IDs,
or sharing instructions. Deserialization constructs a fresh inductive tree;
the tree-size theorem charges both branches of every composition and copy.

## 8. Reduction and normalization

Reduction is call-by-value only as an executable presentation; the logical
normalization theorem concerns the compatible closure of beta, tensor, abort,
and administrative box-composition reductions.

No reduction crosses a box boundary. Composition constructs a quoted proof of
the composed conclusion; introspection quotes the proof of the box itself.
Consequently reduction cannot inspect an unknown proof payload or turn one
payload occurrence into two.

The core normalization proof uses a reducibility interpretation indexed by
type and an affine substitution lemma. Canonical forms imply that a closed
normal term of `zero` cannot exist in the base profile. The checker-adequacy
theorem transfers this result to raw proof certificates.

## 9. Computational G2 interface

The abstract layer formalizes a consequence relation with affine implication,
box, and explicit derivations of the Loeb operations:

```text
L1 : box (A lolli B) |- box A lolli box B
L2 : box A           |- box (box A)
L3 : |- A            -> |- box A
```

It also takes the selected Goedel-Jeroslow equivalence
`G <-> box (G lolli zero)` and box-restricted contraction. Following
Beklemishev-Shamkanov, the proof constructs the refutability/provability
steps explicitly and yields the formalized G2 derivation. At the term level,
supplying both `self0` and `copy-box` produces `boom : zero`.

The theorem is coding-independent in a checkable architectural sense:
`Reflection.AbstractG2` imports only native syntax, derivations, and core
metatheory. Its import closure is audited and contains no machine or numeric
code module.

## 10. Structural preservation of `self0`

The baseline proof is not “we failed to find `zero`.” It defines an
elimination transformation on any hypothetical closed derivation of `zero` in
`Base + self0`.

After normalization, a principal use of `self0` must consume
`quote S q : box S zero`. The complete proof `q : zero` is physically present
inside that quote and is a strict constructor-descendant of the enclosing
normal derivation. Elimination recursively replaces that use using the result
for `q`. The recursion is justified by the well-founded proper-subtree
relation.

The proof must cover every normal-form constructor, modal administrative rule,
and fixed-point constructor. If any rule can synthesize `box S zero` without
containing its complete source proof as a strict descendant, the theorem fails
and the design must be revised.

The final statement is:

```text
Deriv (Base + self0) [] zero -> Deriv Base [] zero.
```

Combined with base consistency, it establishes consistency of the exact sealed
baseline relative to Lean's metatheory. `self1` follows by applying the
accepted contract proof and its complement to obtain `box S zero`, then
consuming it with `self0`.

## 11. Non-definability and the matched control

The baseline non-definability theorem is parametric:

```text
not exists t,
  forall S A,
    Closed t /\ HasType baseline t
      (box S A lolli tensor (box S A) (box S A)).
```

Its first proof target is a free affine resource model in which an assumption
has multiplicity one, tensor adds multiplicities, lolli is resource-sensitive,
and every baseline modal primitive has output box multiplicity at most its
input multiplicity. The requested type has output multiplicity two from input
multiplicity one, so it has no interpretation and hence no term. A logical
relation connects typing to the model.

The control adds only `copy-box`; all other grammar and rules are shared by
construction. A structural diff theorem proves that the sealed profiles differ
only at that capability. `copy-box` supplies box-contraction, abstract G2
instantiates, and a raw serialized `boom` certificate must pass the copy
checker. The identical bytes must fail baseline checking at the copy node.

## 12. Protected Level-1 contracts

`Pi1Contract` is a native record containing program and finite-observation
syntax plus a decidable bad-observation predicate. It denotes the universal
claim that no finite observation is bad; it does not denote termination.

`Clash1 S` contains:

- the exact protected contract syntax;
- a checked closed `S` proof of that contract; and
- a checked closed `S` proof of its declared syntactic complement.

`collapse : Clash1 S lolli box S zero` composes those payloads into an explicit
boxed contradiction. `self1` is `self0` after `collapse`. Neither construction
asserts that all protected contracts are true.

## 13. Turing-complete representability

The machine layer uses finite native data:

```text
Program := finite transition table over structural State and Symbol atoms
Config  := state * left-tape-list * head-symbol * right-tape-list
Step    := structural application of one transition
Trace   := nonempty list of Config values
```

The executable `checkTrace` verifies initial configuration, every adjacent
step, and the declared terminal observation. Soundness and completeness are
proved against the inductive `Step` and reflexive-transitive execution
relations.

Turing completeness is established by a structural translation from a
standard deterministic single-tape Turing machine presentation. Programs and
states are native constructor trees, not numbers. The correspondence theorem
preserves and reflects one step and therefore finite runs. Since the source
model is Turing complete, arbitrary partial computations are representable.

There is no total `run : Program -> Input -> Result`. A fuel-bounded host
driver may demonstrate prefixes, and an explicit coinductive/partial layer may
describe divergence, but neither eliminates into proof types.

## 14. CLI

The compiled executable accepts one S-expression request and emits one
canonical S-expression response. Operations are:

- `parse`, `print`, `typecheck`, and `normalize`;
- `check-proof` and `check-trace`;
- `system-id` and `compare-system-id`;
- `demo-self0`, `demo-self1`, and `demo-fixed-point`;
- `demo-copy-boom` under the copy identity; and
- `demo-trace` plus corrupted-trace rejection.

Errors are data, never uncaught exceptions in the public path. The CLI reports
which system identity was expected and found without treating a hash as the
identity itself.

## 15. Completion discipline

Paper gate P00 precedes every implementation gate. The calculus paper must
receive an explicit acceptance decision, and existing prototype modules must
then pass a rule-by-rule conformance audit. No executable result obtained
before that review is evidence for normalization, consistency, `self0`
preservation, or copy non-definability.

The exact gates are maintained in the acceptance matrix. A theorem is complete
only when its declaration builds, its dependency list is audited, and its
corresponding executable controls pass where applicable. The AAR must state
any theorem that required a design change and may not translate test success
into a consistency claim.
