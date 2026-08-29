# The Affine Lambda Self-Justification Calculus

## A paper specification for ALSJAS

Status: Pre-mechanization theory draft; implementation gate

Date: 2026-08-29

Governing decision: ADR-0003

Supersedes the calculus sketches in `docs/design/alsjas.md` wherever this
paper is more precise. It does not certify any theorem merely by stating its
proof. Every result marked **mechanization obligation** must eventually be
checked in Lean without project-local axioms.

## Abstract

This paper defines the object calculus that ALSJAS is intended to implement.
The calculus is an intuitionistic affine lambda calculus with tensor, affine
implication, falsehood, proof-carrying necessity, and one selected modal fixed
point. A boxed proposition contains a finite derivation tree for that
proposition under one exact system identity. Boxes may be composed and
positively introspected, but there is no rule that unboxes an arbitrary proof
and no rule that duplicates an unknown box.

Three rule sets are distinguished. The modal base `B[S]` has no native
consistency program. The self-justifying calculus `J[S]` adds the single closed
constant `self0[S] : box[S] 0 -o 0`. The matched control `C[S]` adds, in
addition, `copy[S,A] : box[S] A -o box[S] A tensor box[S] A`. The label `S` is
an opaque but exact structural identity; the deployed baseline and control use
different recursively sealed identities.

The central paper argument is relative. Affine normalization and canonical
forms make `B[S]` consistent. Any hypothetical closed `J[S]` derivation of
`0` can be normalized until its decisive use of `self0` consumes a boxed,
strictly smaller derivation of `0`; well-founded recursion removes such uses.
Thus `J[S]` is consistent whenever `B[S]` is. Adding box contraction destroys
that argument. An explicit Jeroslow-style construction then derives `0` from
`self0` in `C[S]`. A provenance invariant shows why the baseline cannot
duplicate the payload of an arbitrary input box.

The paper also fixes the boundary of the claim. The selected fixed point is a
primitive, structurally named isomorphism, not a fixed-point theorem proved by
arithmetic coding. Turing-complete representability belongs to an independent
machine-and-trace layer and is not used by the consistency or G2 arguments.

## 1. Why this document precedes further code

The earlier ALSJAS design named term constructors and desired theorems, but it
did not give a complete calculus. In particular, it did not provide all of the
following in one normative place:

- a grammar parameterized by the exact system and rule profile;
- a distinction between the modal base, the `self0` extension, and the copy
  control;
- affine typing rules with explicit context splitting;
- the finite derivation carried by a box;
- the reductions that compose and introspect those derivations;
- a precise statement of the copy non-definability theorem;
- a normalization measure compatible with opaque proof payloads; or
- natural-language proofs detailed enough to determine the Lean theorem
  statements.

Writing a checker before fixing those objects reverses the dependency. This
paper is therefore a blocking input to the mechanization, not retrospective
explanation of whatever the current program happens to do.

## 2. Sources and deliberate departures

The modal rules are motivated by the distinction between hypothetical truth
and closed validity in Pfenning and Davies. ALSJAS adopts only the closed
introduction aspect: a box is evidence that a proposition has a complete
closed derivation. It deliberately omits the usual elimination that would add
the underlying proposition as a reusable validity assumption.

The affine context discipline is the multiplicative intuitionistic fragment
with weakening but without contraction. Benton's separation of linear and
non-linear worlds is relevant background, but ALSJAS does not import a
cartesian/non-linear world or an exponential with a comonoid structure.
Consequently `box` is not linear logic's unrestricted `!`.

Beklemishev and Shamkanov isolate box-restricted contraction as sufficient for
the abstract G2 argument and exhibit a contraction-free fixed-point setting in
which formalized G2 can fail. ALSJAS turns that boundary into proof programs:
`C[S]` has precisely a box-contraction program, while `J[S]` does not.

Roberts shows that fixed-point and diagonal arguments can survive in weak
substructural settings. That result warns against the slogan “affinity blocks
diagonalization.” ALSJAS therefore assumes a selected fixed point and proves a
much narrower resource claim: the concrete G2 program written in Section 11
needs two uses of the same boxed refutation, and the matched control supplies
exactly those two uses.

## 3. Parameters and the three calculi

Fix an exact structural system label `S`. At the proof-theory level `S` is an
opaque atom with decidable structural equality. It is not a natural number,
hash, address, or abbreviation for an infinite unfolding.

For that label, define three profiles:

```text
B[S]  modal affine base
J[S]  B[S] + self0[S]
C[S]  J[S] + copy[S,A] for every well-formed A
```

`B[S]` is a metatheoretic erasure calculus. It may be instantiated at the
baseline's own self-referential label even though removing `self0` would yield
a different deployable identity. This is intentional: the preservation
theorem compares rule sets while keeping every modal label inside an existing
proof unchanged.

Accordingly, `B[S_J]` is not a raw-certificate profile accepted by the
`S_J` runtime checker. That checker recognizes `J[S_J]`. `B[S_J]` exists only
as the external target of the `self0`-elimination theorem, where its derivation
relation is defined independently of serialized system authorization.

The deployed identities solve separate finite structural equations:

```text
S_J = seal(mu self. specification-of J[self])
S_C = seal(mu self. specification-of C[self])
```

The `mu` above binds occurrences in designated identity fields only. It is a
finite binder in metadata, not an object-language recursive type former.
Because the copy capability occurs in the second specification,
`S_J != S_C` by structural equality.

Closed constants may occur more than once as syntax. Affinity restricts uses
of open assumptions; it does not prohibit writing the same closed theorem or
primitive constant twice in a finite term.

## 4. Grammar

### 4.1 Types

Let `alpha`, `beta`, ... range over atomic proposition names. The type grammar
for a fixed `S` is:

```text
A, B ::= 0
       | 1
       | alpha
       | A tensor B
       | A -o B
       | box[S] A
       | G[S]
```

`0` has no introduction rule. `1` is the tensor unit. `tensor` is
multiplicative conjunction and `-o` is affine implication. `box[S] A` contains
a complete proof of `A` checked under the exact identity `S`. `G[S]` is the
one selected Gödel-Jeroslow proposition for `S`.

There is no general type-level fixed-point constructor. The only fixed-point
equation exposed by the calculus is:

```text
G[S]  ~=  box[S] (G[S] -o 0).
```

The two directions are primitive typed constructors in Section 6.4.

### 4.2 Contexts

A context is a finite map from distinct variables to types:

```text
Gamma ::= empty | Gamma, x : A
```

Order is immaterial. `Gamma # Delta` means that the variable domains are
disjoint, and `Gamma + Delta` denotes their disjoint union. A context may be
weakened, but no rule contracts two occurrences of an assumption into one.

### 4.3 Terms and embedded derivations

Terms and finite derivations are mutually defined. In `quote[S,D]`, `D` is a
complete finite derivation whose conclusion is stored with the node. It is not
a theorem name, pointer, digest, or host callback.

```text
t, u ::= x
       | unit
       | pair(t, u)
       | letPair(t; x, y. u)
       | lam(x : A. t)
       | app(t, u)
       | abort[A](t)
       | quote[S,D]
       | boxComp(t, u)
       | boxUp(t)
       | gFold[S](t)
       | gUnfold[S](t)
       | self0[S]                  -- J[S] and C[S] only
       | copy[S,A]                 -- C[S] only
```

The grammar contains no general unbox, box iterator, proof recursor, sharing
node, back-reference, cyclic certificate, or unrestricted recursion operator.

The copy constant is indexed by `A` at the metalevel. This is a rule schema,
not runtime type inspection. The base language may also be considered with a
fresh open type variable `alpha`; this paper uses that conservative extension
to state the parametric copy theorem precisely.

### 4.4 Derivation judgment

The primary judgment is:

```text
Gamma |-P[S] t : A
```

where `P` is one of `B`, `J`, or `C`. A derivation object records the profile,
the exact label, every premise, and every context split. The notation
`D : Gamma |-P[S] t : A` names such a finite derivation.

The profile and label on an embedded derivation must match the surrounding
quotation exactly. Cross-profile quotation is not a rule.

## 5. Structural and affine rules

The following rules are common to all three profiles. Explicit weakening is
shown once rather than built into every rule.

### 5.1 Variables, weakening, unit, and falsehood

```text
------------------------------ var
x : A |-P[S] x : A

Gamma |-P[S] t : A     Gamma subseteq Delta
---------------------------------------------- weak
Delta |-P[S] t : A

------------------------------ one-I
empty |-P[S] unit : 1

Gamma |-P[S] t : 0
-------------------------------- abort
Gamma |-P[S] abort[A](t) : A
```

Exchange is definitional because contexts are finite maps. There is no
contraction rule.

### 5.2 Tensor

```text
Gamma |-P[S] t : A     Delta |-P[S] u : B     Gamma # Delta
---------------------------------------------------------------- tensor-I
Gamma + Delta |-P[S] pair(t,u) : A tensor B

Gamma |-P[S] t : A tensor B
Delta, x : A, y : B |-P[S] u : C
Gamma # Delta
---------------------------------------------------------------- tensor-E
Gamma + Delta |-P[S] letPair(t; x,y.u) : C
```

Weakening permits either component or either bound variable to go unused.
Context splitting prevents one open resource from reaching both premises.

### 5.3 Affine implication

```text
Gamma, x : A |-P[S] t : B
---------------------------------------------- -o-I
Gamma |-P[S] lam(x:A.t) : A -o B

Gamma |-P[S] f : A -o B
Delta |-P[S] a : A
Gamma # Delta
---------------------------------------------- -o-E
Gamma + Delta |-P[S] app(f,a) : B
```

Again, `x` may be absent from `t` by weakening, but it cannot occur twice.

## 6. Modal and fixed-point rules

### 6.1 Closed proof-carrying quotation

```text
D : empty |-P[S] p : A
---------------------------------------------- box-I
empty |-P[S] quote[S,D] : box[S] A
```

The conclusion stores `D`, including `p`, rather than merely asserting that a
proof exists. Ambient weakening may later place the closed box in any affine
context. The rule cannot quote an open derivation.

There is deliberately no elimination rule of either form below:

```text
box[S] A |- A              forbidden
box[S] A, (A |- B) |- B    forbidden
```

### 6.2 Boxed composition

```text
Gamma |-P[S] f : box[S] (A -o B)
Delta |-P[S] a : box[S] A
Gamma # Delta
------------------------------------------------------------- box-K
Gamma + Delta |-P[S] boxComp(f,a) : box[S] B
```

This is a proof-program form of the modal K/boxed-composition operation. It
consumes each input box once.

### 6.3 Positive introspection

```text
Gamma |-P[S] t : box[S] A
------------------------------------------------------------- box-4
Gamma |-P[S] boxUp(t) : box[S] (box[S] A)
```

The output contains a proof of the proposition represented by the input box;
it does not retain a second usable copy of the input.

### 6.4 Selected fixed point

```text
Gamma |-P[S] t : box[S] (G[S] -o 0)
------------------------------------------------------------- G-fold
Gamma |-P[S] gFold[S](t) : G[S]

Gamma |-P[S] t : G[S]
------------------------------------------------------------- G-unfold
Gamma |-P[S] gUnfold[S](t) : box[S] (G[S] -o 0)
```

These rules witness the one selected equivalence. They do not allow a program
to form a fixed point of an arbitrary type expression.

## 7. Profile-specific rules

### 7.1 Native Level-0 consistency program

`J[S]` and `C[S]` add exactly:

```text
------------------------------------------------------------- self0
empty |-P[S] self0[S] : box[S] 0 -o 0
```

This is a primitive closed proof program. Its presence is not itself evidence
that the calculus is consistent. The relative theorem in Section 10 is the
required evidence.

### 7.2 Matched copy control

`C[S]` alone adds the schema:

```text
------------------------------------------------------------- copy
empty |-C[S] copy[S,A] :
  box[S] A -o (box[S] A tensor box[S] A)
```

No other formation, typing, modal, fixed-point, or structural rule changes.
In particular, the control does not add general contraction.

## 8. Substitution and reduction

### 8.1 Capture-avoiding affine substitution

`t[u/x]` is the usual capture-avoiding substitution. It does not traverse the
derivation payload of `quote[S,D]`: that payload is a closed proof belonging to
the quotation node, not syntax in the scope of the surrounding binder.

Because typing gives `x` to at most one premise at every split, a well-typed
term contains at most one free occurrence of each context variable.

### 8.2 Principal contractions

The compatible logical reduction relation is generated by:

```text
app(lam(x:A.t), u)
  --> t[u/x]

letPair(pair(t,u); x,y.v)
  --> v[t/x, u/y]

boxComp(quote[S,Df], quote[S,Da])
  --> quote[S, appDeriv(Df,Da)]

boxUp(quote[S,D])
  --> quote[S, quoteDeriv(D)]

gUnfold[S](gFold[S](t))
  --> t
```

Here `appDeriv(Df,Da)` is the closed derivation obtained by affine application
of the conclusions of `Df` and `Da`. `quoteDeriv(D)` is the closed derivation
whose term is `quote[S,D]`. These are structural proof-tree constructors, not
calls to proof search.

No reduction enters `D` beneath a quotation. Reductions may occur in any other
term context. The executable presentation may choose deterministic
left-to-right call by value, but the normalization theorem concerns the
compatible relation above.

The copy profile adds one contraction:

```text
app(copy[S,A], quote[S,D])
  --> pair(quote[S,D], quote[S,D]).
```

This is the only rule that creates two occurrences of an unknown proof
payload.

### 8.3 Normal forms

A term is normal when none of the contractions applies anywhere outside a
quotation. In particular:

- boxed composition of two quoted proofs is not normal;
- positive introspection of a quoted proof is not normal;
- unfolding a folded fixed point is not normal; and
- application of `self0[S]` to a quoted proof is normal, because `self0` is an
  axiom program rather than an evaluator for the payload.

That last normal form is exactly the case handled by structural `self0`
elimination.

## 9. Core metatheory: paper proofs

Every result in this section is a **mechanization obligation**. The arguments
fix the intended induction and the lemmas that the Lean proof must expose.

### Lemma 9.1 — Affine occurrence

If `D : Gamma |-P[S] t : A`, each variable in `Gamma` has at most one free
occurrence in `t` outside quoted payloads.

**Proof.** Induct on `D`. The variable case has one occurrence. Unit and every
closed constant have none. Unary rules inherit the induction hypothesis.
Tensor introduction, application, and boxed composition give disjoint
contexts to their premises, so no variable can occur in both children.
Tensor elimination likewise separates the scrutinee context from the body
context; each newly bound component occurs at most once in the body. Weakening
adds variables with zero occurrences. Quotation is closed and its payload is
outside surrounding binding. No other case can introduce a second occurrence.

### Lemma 9.2 — Weakening and exchange

If `Gamma |-P[S] t : A`, `Gamma` embeds into `Delta`, and the added variables
are fresh, then `Delta |-P[S] t : A`. Renaming context variables preserves
typing.

**Proof.** Weakening is a rule, and exchange is definitional for finite maps.
For a list implementation, prove both by induction on the typing derivation
and transport the recorded context splits.

### Lemma 9.3 — Affine substitution

If

```text
Gamma, x:A |-P[S] t : B
Delta |-P[S] u : A
Gamma # Delta,
```

then `Gamma + Delta |-P[S] t[u/x] : B`.

**Proof.** Induct on the derivation of `t`. If its last rule places `x` in no
premise, erase the unused assumption and weaken by `Delta`; this does not copy
`u`. If the last rule places `x` in one premise, apply the induction hypothesis
only there and reassemble the rule with the untouched disjoint premises. The
context-splitting rules ensure there is no case in which two induction
hypotheses both require `u`. Quotation cannot contain `x` because its premise
is closed. The modal and fixed-point cases are otherwise ordinary congruence
cases.

Tensor substitution is the two-variable iteration of this lemma. The two
components are substituted into disjoint single-use positions.

### Theorem 9.4 — Subject reduction

If `Gamma |-P[S] t : A` and `t --> u`, then
`Gamma |-P[S] u : A`.

**Proof.** Induct on the reduction context. Congruence cases rebuild the
typing rule using the induction hypothesis. Beta and tensor contractions use
Lemma 9.3. For boxed composition, invert both box introductions to recover
closed derivations `Df` of `A -o B` and `Da` of `A`; application constructs a
closed derivation of `B`, which can be quoted. Positive introspection quotes
the existing closed quotation derivation. Fixed-point contraction cancels the
two selected isomorphism rules. The copy rule is type preserving because both
result branches are the two explicitly constructed quotation nodes; it is
available only in `C[S]`.

### Theorem 9.5 — Determinism of the executable step

The left-to-right call-by-value step function returns at most one successor.

**Proof.** Define evaluation contexts with a unique decomposition theorem:
every non-value term is either a single principal redex or has exactly one
leftmost evaluation subterm. Principal redex heads are disjoint. A function
implementing that decomposition is therefore deterministic by construction.
This theorem is about the executable strategy, not confluence of all
compatible reductions.

### Theorem 9.6 — Strong normalization of `B[S]` and `J[S]`

Every well-typed `B[S]` or `J[S]` term is strongly normalizing under the
compatible reduction relation.

**Proof.** Define the *active size* `a(t)` as ordinary constructor size except
that `a(quote[S,D]) = 1`; the derivation payload is opaque to reduction.
Lemma 9.1 implies the following size equation for substitution:

```text
if x occurs once in t, a(t[u/x]) = a(t) - 1 + a(u);
if x is unused,          a(t[u/x]) = a(t).
```

Thus beta contraction strictly decreases active size: the lambda and
application nodes disappear, and `u` is inserted no more than once. The same
argument handles tensor elimination because each bound component is used at
most once. Boxed composition replaces two quotation nodes and the composition
node by one opaque quotation node. Introspection replaces an active
introspection/quotation pair by one opaque quotation. Fixed-point contraction
removes fold/unfold nodes. A compatible step in a proper active subterm also
strictly decreases the total.

Every `B[S]` or `J[S]` step therefore strictly decreases a natural number, so
there is no infinite reduction. The proof does not apply unchanged to `C[S]`:
the copy contraction duplicates a quotation and intentionally breaks the
single-use substitution invariant. No consistency result relies on
normalization of `C[S]`.

### Lemma 9.7 — Closed canonical forms

For `P` equal to `B` or `J`, a closed normal term either has an introduction
form appropriate to its type or contains a proper closed normal subterm typed
at `0`. The second alternative includes a direct `abort[A](n0)` and an
elimination blocked by such a term. Subject to that explicit *zero-bearing*
alternative:

1. A normal term of `1` is `unit`.
2. A normal term of `A tensor B` is `pair(t,u)` with normal components.
3. A normal term of `A -o B` is a lambda or a profile constant with that exact
   function type.
4. A normal term of `box[S] A` is `quote[S,D]`.
5. A normal term of `G[S]` is `gFold[S](quote[S,D])`.
6. A closed normal `B[S]` term of `0` does not exist.
7. A closed normal `J[S]` term of `0` either contains a proper closed normal
   subterm of `0`, or has the head form `app(self0[S], quote[S,D0])`, with
   `D0 : empty |-J[S] p : 0`.

**Proof.** Use induction on normal terms together with inversion of their
typing derivations. A closed neutral cannot be headed by a variable. A lambda
application and tensor elimination of a pair would be redexes. Boxed
composition and introspection of closed canonical boxes would be redexes.
Unfolding a closed canonical `G[S]` would be a redex. If one of those
eliminations is instead blocked by a noncanonical closed input, follow its
neutral spine: it either reaches an introduction redex or exposes a proper
closed normal subterm of `0`. No base constant has an unboxed result `0`. The
only added `J[S]` constant whose eventual codomain is `0` is `self0[S]`; if its
box argument is not a quotation, the same neutral-spine analysis exposes a
proper normal-zero subterm. This proves the classification by induction on
normal-term size. For clause 6, choose a smallest hypothetical closed normal
`B[S]` term of `0`: it has no `self0` head, while every blocked form exposes a
strictly smaller term of `0`, contradicting minimality.

### Corollary 9.8 — Consistency of the modal base

There is no closed `B[S]` derivation of `0`.

**Proof.** Normalize a hypothetical derivation using Theorems 9.4 and 9.6.
The result would be a closed normal term of `0`, contradicting Lemma 9.7(6).

This is proof-theoretic consistency relative to the metatheory in which the
normalization and canonical-forms arguments are formalized.

## 10. Structural preservation of `self0`

### 10.1 Hereditary proof size

Active size deliberately ignores quoted derivations, so a second measure is
needed for self-justification. Let `h(D)` be the number of inference nodes in
the complete hereditary derivation tree: the premise embedded by every quote
is counted recursively. Raw certificates are finite trees, so `h(D)` is a
natural number and every embedded quotation premise has strictly smaller
height than the derivation containing the quote.

### Lemma 10.1 — Normalization does not increase hereditary height

Each `B[S]` or `J[S]` principal contraction can be lifted to derivations so
that hereditary height does not increase.

**Proof.** Affine beta and tensor substitution insert each premise derivation
at most once. Boxed composition starts with two quoted derivations plus the
composition inference and replaces them by one quote whose payload has one
application inference over those same two derivations; the outer composition
and quote overhead pays for the new application node. Positive introspection
repackages the same derivation beneath one additional quote, replacing the
introspection node, so height is unchanged or smaller. Fixed-point contraction
removes rules. Congruence preserves the comparison. Iterating the result gives
a normal derivation of height no greater than the source derivation.

This lemma must be stated over derivations, not only terms, because quotation
premises are part of the measure.

### Theorem 10.2 — `self0` elimination at contradiction

For every exact label `S`:

```text
empty |-J[S] t : 0
implies
there exists u such that empty |-B[S] u : 0.
```

**Proof.** Use well-founded induction on `h(D)`, where
`D : empty |-J[S] t : 0`.

Normalize `D` to `N`. By Lemma 10.1, `h(N) <= h(D)`. Apply the normal-zero
classification in Lemma 9.7(7).

- If `N` contains a proper closed normal subderivation at type `0`, apply the
  induction hypothesis to that subderivation.
- Otherwise the normal term is `app(self0[S], quote[S,D0])`. The quotation
  rule gives
  `D0 : empty |-J[S] p : 0`. The complete `D0` tree is a strict descendant of
  the quote premise and hence `h(D0) < h(N) <= h(D)`. Apply the induction
  hypothesis to `D0` and return the resulting `B[S]` derivation.

No other normal form has type `0`. Every recursive call follows an actual
proper-child relation in the finite proof tree; no Gödel numbering or numeric
bound on encoded proofs is used to define the descent.

### Corollary 10.3 — Consistency preservation

If `B[S]` is consistent, then `J[S]` is consistent.

**Proof.** A `J[S]` contradiction would give a `B[S]` contradiction by
Theorem 10.2, contrary to Corollary 9.8.

For the deployed baseline, instantiate `S` with `S_J`. This yields the precise
claim intended by “ALSJAS proves its own consistency”: the exact calculus
whose sealed identity is `S_J` contains the closed program
`self0[S_J] : box[S_J] 0 -o 0`, and the external metatheory proves that adding
that program to its erased modal base preserves consistency.

It does not prove soundness of all ALSJAS propositions, truth of all protected
contracts, or absolute consistency independent of Lean's metatheory.

## 11. The matched copy control and computational G2

This section gives the full paper derivation that motivates the control. It is
not enough to cite an abstract theorem: each intermediate type identifies
where the duplicated box is consumed.

Write:

```text
Box A  = box[S] A
G      = G[S]
Not A  = A -o 0
N      = Not G
s      = self0[S] : Box 0 -o 0
```

The selected fixed point gives:

```text
gFold   : Box N -o G
gUnfold : G -o Box N.
```

### Lemma 11.1 — Jeroslow map with box contraction

In `C[S]` there is a closed term:

```text
j : G -o Box 0.
```

**Construction.** Assume `g : G` exactly once.

1. `q = gUnfold(g) : Box N`.
2. `copy[S,N](q)` yields `q1 : Box N` and `q2 : Box N`.
3. `boxUp(q1) : Box (Box N)`.
4. The closed fold function `gFold : Box N -o G` can be quoted, giving
   `Box (Box N -o G)`.
5. Boxed composition of steps 3 and 4 yields `Box G`.
6. `q2 : Box (G -o 0)` composed with `Box G` yields `Box 0`.

The two occurrences produced in step 2 have distinct affine destinations.
Without that step, positive introspection would consume the only `Box N`,
leaving no refutation to compose with `Box G`.

### Lemma 11.2 — Formalized G2

In `C[S]` there is a closed term:

```text
F : Box (Box 0 -o 0) -o Box 0.
```

**Construction.** Assume `hs : Box (Box 0 -o 0)`.

The ordinary affine composition combinator has closed type:

```text
(Box 0 -o 0) -o ((G -o Box 0) -o (G -o 0)).
```

Quote that combinator. Box-compose it first with `hs`, then with the quoted
closed Jeroslow map `j`. The result is `Box (G -o 0)`. Fold it to obtain `G`,
then apply `j` to obtain `Box 0`. Every open assumption is consumed once.

### Theorem 11.3 — Contradiction from native consistency plus copy

`C[S]` has a closed term of `0`.

**Construction.** Since `s` is closed, quote its complete derivation to obtain
`Box (Box 0 -o 0)`. Apply `F` to obtain `Box 0`, then apply `s` to obtain `0`.

The syntax of the closed constant `s` appears twice—once inside a finite quote
and once as the final function. This is allowed because no open assumption is
contracted. The only duplication of an unknown open proof resource occurs in
Lemma 11.1 and is exactly `copy[S,N]`.

### Corollary 11.4 — G2 exclusion form

Any consistent calculus satisfying these modal operations, the selected fixed
point, and box contraction cannot contain a closed inhabitant of
`Box 0 -o 0`.

**Proof.** Such an inhabitant feeds the construction above and produces a
closed contradiction.

This is the computational specialization of the box-contraction boundary in
Beklemishev and Shamkanov. It does not derive the selected fixed point; it is
conditional on the primitive fold/unfold interface.

## 12. What “copy is not definable” must mean

The previous design stated a polymorphic type but supplied no polymorphic
grammar. That statement was not yet a theorem. Two complementary formulations
are required.

### 12.1 Open-type non-inhabitance

Extend the type grammar conservatively with a fresh type variable `alpha` that
does not occur in any constant type or system declaration.

### Theorem 12.1

There is no `J[S]` derivation:

```text
x : box[S] alpha
  |- t : box[S] alpha tensor box[S] alpha.
```

Equivalently, there is no term schematic in `alpha` of type:

```text
box[S] alpha -o
  (box[S] alpha tensor box[S] alpha).
```

**Proof.** Invert a normal derivation of the tensor conclusion. It must end in
`pair(t1,t2)`, whose premises receive disjoint contexts. At most one premise
can receive `x`; the other must be a closed term of `box[S] alpha`. By box
canonical forms, that closed term must contain a closed derivation of `alpha`.
Normalization and atomic canonical forms show that no such derivation exists:
there is no introduction rule or constant for the fresh atom. Contradiction.

This theorem rules out a uniform type-parametric copy program without adding
runtime polymorphism to the language.

### 12.2 Unknown-payload provenance

Type alone is not the whole claim. If a concrete `A` is already provable, a
term may consume its input box once and independently quote another proof of
`A`, producing two boxes of the same type. That does not duplicate the unknown
input payload.

Tag one input quotation payload with a fresh provenance marker `rho`. Define
`mu_rho(t)` as the number of hereditary occurrences of that marker in the
complete output proof tree.

### Theorem 12.2 — Baseline provenance non-duplication

For any well-typed `B[S]` or `J[S]` one-hole program context that receives one
box carrying `rho`, every reduction result has `mu_rho <= 1`.

**Proof.** Induct over typing and reduction. Affine substitution inserts the
tagged input into at most one free occurrence. Tensor and application split
contexts. Boxed composition places each consumed proof tree exactly once in
the composed derivation. Positive introspection nests the input proof once.
Quotation of a closed derivation cannot mention the fresh runtime marker.
Fold/unfold preserve multiplicity. No baseline rule increases it.

The control reduction is the unique counterexample:

```text
copy[S,A](quote[S,D_rho])
  --> pair(quote[S,D_rho], quote[S,D_rho]),
```

whose output has multiplicity two.

Together, Theorems 12.1 and 12.2 make “copy” precise: the baseline has neither
a uniform inhabitant of the copy type nor an operation that duplicates the
identity of an arbitrary proof payload. It is not claimed that every
concrete, inhabited type lacks some term with the same simple input/output
type.

## 13. Raw certificates and checker adequacy

The intrinsic calculus above is primary. A raw certificate is its untrusted
serialization, not the definition of derivability.

### 13.1 Canonical raw grammar

At the serialization layer:

```text
sexpr  ::= atom | "(" sexpr* ")"

type   ::= 0
         | 1
         | (atom name)
         | (tensor type type)
         | (lolli type type)
         | (box system-ref type)
         | (godel system-ref)

term   ::= (var natural)
         | (triv)
         | (pair term term)
         | (let-tensor term term)
         | (lam type term)
         | (app term term)
         | (abort type term)
         | (quote system-ref type certificate)
         | (box-comp term term)
         | (box-introsp term)
         | (godel-fold system-ref term)
         | (godel-unfold system-ref term)
         | (self0 system-ref)
         | (copy-box system-ref type)       -- C only

certificate ::= (proof sealed-system type term)
```

The implementation may use de Bruijn indices, but the paper judgment remains
alpha-invariant. Natural numbers here are structural variable indexes, never
codes for formulas, proofs, systems, or programs.

Every nested certificate is a literal proper subtree of its enclosing raw
quotation node. The grammar has no node identifier or sharing form.

### 13.2 Checker contract

For an externally selected sealed system `S`, the checker must:

1. parse the entire certificate;
2. reseal its included system source;
3. compare the complete source structurally with `S`;
4. reject every stale modal reference;
5. enforce the exact profile's constructor set;
6. reconstruct an affine intrinsic derivation; and
7. recursively check every quotation payload.

Recursive quotation checking uses fuel equal to one plus the raw AST node
count. Since each recursive payload is a literal proper subtree, fuel strictly
decreases along every checker call. Rendered string length is not part of the
metatheorem.

### Theorem 13.1 — Checker soundness

```text
check(S, raw) = ok(A,t)
implies
empty |-P[S] t : A.
```

**Proof.** Induct on checker fuel. Parsing and exact identity checks establish
the indices. Each successful inference clause maps to the corresponding
intrinsic constructor. At a quote, the recursive check has smaller fuel, so
the induction hypothesis supplies the embedded closed derivation.

### Theorem 13.2 — Checker completeness

```text
D : empty |-P[S] t : A
implies
check(S, encode(D)) = ok(A,t).
```

**Proof.** Induct mutually on terms and embedded derivations. Canonical
serialization round trips by structural induction. Each intrinsic constructor
selects the matching inference clause; affine context splits become usage
certificates. For quotation, the embedded derivation is a strict subtree and
the mutual induction hypothesis validates it. The raw node-count bound exceeds
the maximum quotation depth by a direct subtree-height lemma.

Soundness is security-critical. Completeness prevents the raw format from
silently defining a weaker calculus than this paper.

## 14. System identity and the matched comparison

The sealed system source must include, in canonical order:

- the format and checker versions;
- the exact grammar and rule profile;
- the modal operations;
- the selected fixed-point declaration;
- the `self0` declaration when present;
- protected-contract declarations when present; and
- the copy capability when present.

Sealing validates the one restricted `mu self` binder and returns the complete
finite source tree. Equality compares those trees. A display hash may summarize
an identity but can never authorize a proof.

### Theorem 14.1 — Matched identity difference

`S_J != S_C`, and after replacing the bound self marker by a common variable,
their canonical capability lists differ in exactly the `copy-box` entry.

**Proof.** Unfold both canonical source constructors. Every field is
definitionally identical except the profile name and the appended capability.
Structural list comparison detects those nodes. The recursive labels differ
because each finite source contains its own profile under the binder.

The contradiction certificate from Section 11 must be regenerated under
`S_C`. Submitting the identical bytes to the `S_J` checker fails first at exact
identity comparison or, if deliberately relabeled and resealed, at the copy
constructor. Both negative controls are required.

## 15. Protected Level-1 contracts

The minimal calculus does not need a primitive `Clash1` type. The protected
contract layer is a conservative extension built after Sections 9–14 are
mechanized.

A Level-1 clash package must contain the contract syntax and two checked closed
derivations whose conclusions are a declared syntactic complement pair. A
structural `collapse1` program consumes that package and constructs
`box[S] 0`; it does not assert either contract is semantically true. The
derived program is:

```text
self1[S] = lam(c. app(self0[S], collapse1[S](c)))
```

Its consistency justification is inherited from Theorem 10.2 only after the
collapse checker is proved sound. It is not part of the minimal grammar and
must not be smuggled into the base-consistency proof as an unexplained
constant.

## 16. Turing-representability boundary

Nothing in Sections 3–15 requires a universal evaluator, numeric program code,
or a theorem that all computations terminate.

The separate machine layer may define finite structural values:

```text
Machine, Configuration, Transition, Trace
```

and prove that a decidable trace checker is sound and complete for finite
runs. A structural translation from a standard deterministic Turing-machine
model may then preserve and reflect one step and every finite run. That proves
representation of arbitrary partial computations because a nonhalting source
machine has arbitrarily long finite prefixes, not because ALSJAS constructs a
total proof-producing evaluator.

The dependency direction is one way:

```text
minimal calculus and reflection   machine syntax and traces
              \                   /
                       CLI
```

No consistency, self0-preservation, G2, or copy-nondefinability theorem may
import the machine branch.

## 17. Theorem ledger and proof order

The paper imposes this order on the mechanization:

| Gate | Paper result | Required formal evidence |
| --- | --- | --- |
| P01 | Grammar and profile separation | Inductive types/rules matching Sections 3–7 |
| P02 | Affine occurrence | Lemma 9.1 |
| P03 | Weakening/exchange | Lemma 9.2 |
| P04 | Affine substitution | Lemma 9.3 |
| P05 | Subject reduction | Theorem 9.4 |
| P06 | Executable determinism | Theorem 9.5 |
| P07 | Baseline/J normalization | Theorem 9.6 |
| P08 | Canonical forms | Lemma 9.7 |
| P09 | Base consistency | Corollary 9.8 |
| P10 | Hereditary-height monotonicity | Lemma 10.1 |
| P11 | `self0` elimination | Theorem 10.2 |
| P12 | Exact baseline consistency | Corollary 10.3 at `S_J` |
| P13 | Jeroslow map | Lemma 11.1 as an affine derivation |
| P14 | Formalized G2 | Lemma 11.2 as an affine derivation |
| P15 | Copy contradiction | Theorem 11.3 checked under `S_C` |
| P16 | Open-type no-copy | Theorem 12.1 |
| P17 | Payload provenance | Theorem 12.2 |
| P18 | Checker adequacy | Theorems 13.1 and 13.2 |
| P19 | Matched identities | Theorem 14.1 and negative certificates |

Dependencies are strict. In particular, executable demos do not precede or
discharge P07–P12, and the copy demo does not discharge P16–P17.

## 18. Consequences for the existing prototype

Before implementation resumes, the current code must be reviewed against the
following paper decisions:

1. The intrinsic relation must have explicit `B`, `J`, and `C` profiles. A
   Boolean hidden inside an adequacy proof is not the grammar.
2. The executable baseline may expose `self0`, but base consistency must be
   stated over `B[S_J]`, which has the same label and no `self0` rule.
3. Boxed composition and positive introspection need structural proof-payload
   contractions, or the canonical-box and self0-elimination proofs must use an
   equivalent explicit derivation normalization. Merely declaring these terms
   to be values is not this calculus.
4. The abstract G2 interface must enforce affine use. A host-language function
   field of type `Program A -> Program B` does not by itself prevent its
   argument from being duplicated.
5. Checker completeness must be proved against the intrinsic derivation
   relation and a structural AST bound, not inferred from examples.
6. The no-copy result must use the open-type and provenance statements in
   Section 12. Absence of a syntax constructor is insufficient, while a simple
   type-only statement for an inhabited concrete proposition is too strong.
7. The selected fixed point must remain visibly primitive. No documentation
   may imply that ALSJAS derives it from Turing completeness or from an
   arithmetic-free diagonal lemma that has not been proved.

Any existing code that conflicts with these decisions is exploratory evidence,
not the specification, and must be revised or retired after paper review.

## 19. Non-claims and open review questions

This draft does not claim:

- that the natural-language proofs are already machine checked;
- that the deployed checker currently implements every rule above;
- that `C[S]` is normalizing;
- that every proposition proved by `J[S]` is true;
- that every concrete type lacks a term with the copy input/output type;
- that the selected fixed point is derived rather than postulated as a typed
  isomorphism;
- that self-consistency removes the external trust in Lean; or
- that Turing-complete representability supplies proof inhabitants by
  divergence.

The paper must receive an explicit review decision on these questions before
the theory is frozen:

1. Is the primitive selected fixed point an acceptable assumption for the
   intended “arithmeticization-free” claim?
2. Is proof-carrying closed quotation, without any general unbox, the intended
   modality?
3. Should fixed-point eta reduction `gFold(gUnfold(g)) --> g` be included, or
   is the beta direction used here sufficient?
4. Is contradiction preservation at `0` sufficient, or is a broader
   conservativity theorem required?
5. Should Level-1 contracts remain a later conservative extension rather than
   a primitive type in the minimal paper?
6. Is the provenance theorem the intended observational meaning of “same
   unknown box,” and what observations may distinguish two proof payloads?

Until these questions are answered and Sections 3–12 are accepted, additional
checker, CLI, reflection-demo, and Turing-layer implementation is premature.

## References

1. Lev D. Beklemishev and Daniyar S. Shamkanov, “Some Abstract Versions of
   Gödel's Second Incompleteness Theorem Based on Non-Classical Logics,” 2016.
   In particular, Definition 3.4, Theorem 3, Remark 3.9, and the
   contraction-free fixed-point system of Section 4.
   <https://arxiv.org/abs/1602.05728>
2. Frank Pfenning and Rowan Davies, “A Judgmental Reconstruction of Modal
   Logic,” *Mathematical Structures in Computer Science* 11(4), 2001.
   <https://www.cs.cmu.edu/~fp/papers/mscs00.pdf>
3. P. N. Benton, *A Mixed Linear and Non-Linear Logic: Proofs, Terms and
   Models*, University of Cambridge Computer Laboratory Technical Report 352,
   1994. <https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-352.html>
4. David Michael Roberts, “Substructural Fixed-Point Theorems and the Diagonal
   Argument: Theme and Variations,” *Compositionality* 5(8), 2023.
   <https://doi.org/10.32408/compositionality-5-8>
