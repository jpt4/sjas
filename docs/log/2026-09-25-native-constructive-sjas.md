# A native constructive SJAS with program extraction

Date: 2026-09-25.

Objective: devise a constructive variant of a self-justifying axiom system.
The user clarified that constructive means a constructive logic, suitable for
program extraction, rather than a requirement that its external consistency
proof itself avoid classical reasoning.

This note gives an intuitionistic construction and its mathematical
consistency and extraction arguments. It changes the deduction method and
forms self-consistency for that new method. It does not merely translate a
classical self-consistency sentence into a target type theory.

Status: paper construction. The proof representation uses the bounded
arithmetization results identified below; no concrete byte-level checker,
extractor implementation, or machine-checked metatheorem is supplied here.
Those implementation tasks are not prerequisites for the mathematical
construction, but would require an ADR and additional validation before an
executable system could be claimed.

## 1. Required properties

The system, denoted I, must have:

1. Intuitionistic first-order inference, including implication, conjunction,
   disjunction, both quantifiers, equality, and falsity.
2. A faithful effective proof predicate for its entire axiom set, including
   the new self-consistency axiom.
3. Both an internal self-consistency assertion and external consistency.
4. An effective transformation of proofs into computational realizers,
   including correct total witness functions for bounded arithmetic outputs.

The arithmetic and proof representation must remain weak enough for these
requirements to coexist. Intuitionistic logic alone does not evade G2.

## 2. Language and ordinary axioms

Use Willard's non-growth arithmetic language. Its numerical primitives are
computable total functions satisfying

```text
f(n_1, ..., n_k) <= max(2, n_1, ..., n_k).
```

The grounding operations include truncated subtraction, rounded division,
maximum, predecessor, logarithm, roots, and bit counting, with the source's
totalizations at boundary inputs. Addition and multiplication are represented
by bounded graph formulas; they are not total function primitives.

Add names C_0, C_1, C_2, ... with the standard interpretation

```text
C_0 = 0;
C_i = 2^(i-1), for i >= 1.
```

As in Willard 2006, the ordinary axioms include a finite description of
C_0, C_1, C_2 and the individually listed naming axioms

```text
a_i : Add(C_i, C_i, C_(i+1)), for i >= 2.
```

These are a schema of separate finite sentences, not a numerical function
symbol mapping a variable i to C_i. They do not assert uniform successor,
addition, or exponentiation totality.

Let B also contain a fixed effective family F of true sentences of the form

```text
forall x_1 ... x_k. D(x_1, ..., x_k),
```

where D is bounded and the only named numerical constants in F are 0, 1,
and 2. This is Willard's "2-reduced" restriction. Grounding-function axioms
from his Group 1 are an admissible choice. His Group 2 axioms can also be
used under their stated truth and proof-representation hypotheses. The
consistency argument below works for every such F with the stated effective
axiom-recognition interface; it does not require a truth oracle to recognize
axioms. An r.e. axiom family uses supplied enumeration witnesses in proofs.

The 2-reduced restriction is essential to the finite-model proof. Arbitrary
true axioms involving the larger C_i cannot simply be added unchanged.

For a completely explicit finite example, take F to contain the universal
closures of the following formulas, using the indicated grounding functions:

```text
Predecessor(x) = Subtract(x,1)
Subtract(x,0) = x
Subtract(x,x) = 0
Divide(x,1) = x
Divide(x,2) <= x
x <= Maximum(x,y) and y <= Maximum(x,y)
Maximum(x,y) = Maximum(y,x)
(x <= y and y <= z) -> x <= z
x <= y or y <= x
not (0 = 1).
```

Every listed formula is true, bounded, and 2-reduced. Membership in this
finite family is a syntactic check. The construction also permits the richer
source grounding axioms; its proof is not limited to this illustrative F.

## 3. Constructive inference and proof representation

Use an ordinary intuitionistic Hilbert presentation of first-order logic
with equality. Its propositional schemes include

```text
A -> B -> A
(A -> B -> C) -> (A -> B) -> A -> C
A -> B -> (A and B)
(A and B) -> A
(A and B) -> B
A -> (A or B)
B -> (A or B)
(A -> C) -> (B -> C) -> (A or B) -> C
FALSE -> A
```

For quantifiers use the schemes

```text
(forall x. A(x)) -> A(t)
A(t) -> exists x. A(x)
(forall x. (A -> B)) -> (A -> forall x. B)   [x not free in A]
(forall x. (A -> B)) -> ((exists x. A) -> B) [x not free in B],
```

with capture-avoiding substitution. Include equality reflexivity and
substitutivity. The rules are modus ponens and universal generalization,
with the usual prohibition on generalizing a variable free in an undischarged
assumption. The nonlogical axioms above are closed. In particular, implication
application remains available. No excluded-middle or double-negation
elimination scheme is admitted for arbitrary formulas.

Proof records are finite sequences with complete formulas, rule tags,
backward references, and the relevant side-condition certificates. Each
axiom application includes the actual axiom formula. Formula sharing and
unexpanded proof macros are not part of this checker.

Fix the following conservative coding convention to make the size estimate
in Section 5 exact rather than asymptotic:

- Keep the source's formula encoding. Annotate each proof-record occurrence
  of C_i with a matching unary index. Thus its proof record uses at least i
  bits, on the ordinary bit-length scale of the integer denoted by C_i.
  This adds explicit size information to the compressed name; it does not
  change the source's formula codes or substitution convention.
- The raw proof record includes every line's complete formula and at least
  one bit per line.
- To obtain its positive numerical proof code p, prepend a leading 1 and
  128 zero bits, and replace each bit of the raw record by four copies.

Writing N for the raw record's length, this ensures

```text
floor(log_2 p) = 128 + 4N.
```

Additional supplied verification traces, if needed for the bounded checker,
are included in the raw record and therefore only increase N. This is an
external encoding operation, not an internally asserted total arithmetic
function. Its constant-factor bit repetition has a bounded verifier.

The raw record contains a source-format Hilbert record q and these index
annotations u. Its rule checker is the intuitionistic restriction of a
classical Hilbert checker. Choose the classical presentation to include the
listed intuitionistic schemes and additional classical schemes with
separate rule tags. Deleting the latter tags adds a bounded syntactic check;
it does not add a semantic acceptance condition. In particular define

```text
ExtraPrf_I(B,h,a,p) :=
  exists q,u <= p.
    Record(p,q,u)
    and ExtraPrf_classical(B,h,a,q)
    and IntOnly(q)
    and MatchingUnaryIndices(q,u).
```

Record checks the delimited fields, fixed prefix, and fourfold bit repetition.
IntOnly checks the rule tags. MatchingUnaryIndices checks the annotations
against the names actually occurring in q. All are bounded bit conditions;
each decoded field is at most p. Together with the source's bounded
ExtraPrf predicate, closure under bounded quantification and conjunction
gives a bounded formula for exactly the accepted intuitionistic proofs of
a from B plus the one additional axiom h. There is one logical derivation q,
not a pair of separate classical and constructive derivations.

If the selected axiom presentation uses enumeration witnesses, these are
fields of p rather than an unbounded search performed by this formula.

Here bounded representability is an imported arithmetization result, not a
consequence of a checker's merely being computable. Willard 2006, Lemma 1,
uses bounded Subst/ExtraPrf predicates and refers to the detailed LinH
construction in Willard 2001, Appendices B-D. The displayed definition reduces
the modifications to syntactic restriction and bounded record coding, not a new
unbounded proof-search algorithm. The optional trace interface must check
local transitions using bounded code fields; supplying a trace is distinct
from proving that all searches terminate.

## 4. Form self-consistency for this checker

Apply the same self-substitution construction to ExtraPrf_I. This produces
a particular closed bounded-universal sentence H and its faithful predicate
Prf_I such that, in the standard interpretation,

```text
I = B + H, using intuitionistic Hilbert deduction;

H says: for all a,p,
        not (FalseCode(a) and Prf_I(a,p)).
```

FalseCode is a bounded formula identifying the code of logical FALSE.
Targeting this fixed contradiction instead of the source's 0=1 does not
change the coding or finite-model argument. All free parameters used in
the self-substitution construction are closed by explicit numeral terms.
H therefore uses only finitely many names C_i.

In detail, start with a template Gamma(g) whose proof predicate admits
either a B-proof or a proof from B plus an axiom h satisfying Subst(g,h).
Instantiate its g by a term denoting the code of Gamma. As in Willard's
construction, the substituted axiom code h is bounded by the supplied proof
code: when the extra axiom is used its complete formula occurs in that
proof; otherwise the B-proof alternative applies. The resulting H is the
additional axiom recognized by its own checker.

This construction does not assume a theorem saying that every numerical
substitution or proof-building operation is total inside I. It fixes one
finite self-referential sentence externally and uses its bounded verifier.

I proves H because H is one of its axioms. Its proof records also contain H
as an ordinary axiom line, so H's predicate covers proofs using H itself.
There is no inherited axiom Con(W) about a different classical system.

## 5. Consistency: the finite-model argument

This is an adaptation of the proof of Willard 2006, Theorem 3, pp. 19-21.
It uses Boolean soundness of intuitionistic inference, not classical rules
inside I. The unary name coding and explicit proof padding above make the
size comparison below immediate.

Assume that an accepted I-proof of FALSE exists, and choose one with the
least numerical proof code p. Let r be at least 2 and at least every index
of a name C_i occurring anywhere in H. Let L be the number of naming-axiom
applications in p.

Here H means its fully expanded arithmetic formula, including the bounded
proof-predicate definitions and all numeral terms. Abbreviations cannot
hide a larger constant index from this count.

The proof p must use H: the standard natural numbers satisfy every axiom
of B, and intuitionistic inference is sound in that structure. Hence the
raw record contains the entire formula H. The coding convention gives

```text
r <= N, after choosing r = max(2, largest name index in H);
L <= N;
floor(log_2 p) = 128 + 4N > r + L + 1.
```

Choose the least k >= r for which the naming axiom a_(k+1) does not occur
in p. At most L consecutive candidates can occur, so

```text
k <= r + L.
```

Now form a finite classical structure M with domain {0,...,2^k}. Interpret
the grounding functions by their usual numerical operations on that domain,
and interpret the names by

```text
C_i = 2^(i-1), for 1 <= i <= k+1;
C_i = 0,      for i = 0 or i >= k+2.
```

All grounding functions stay inside the domain because they are non-growth.
Every naming axiom holds except a_(k+1), which was chosen not to occur in p.
The initial axioms for 0,1,2 hold as well.

Every axiom from F holds in M. Its constants are 0,1,2, and its bounded
matrix is absolute between M and the standard natural numbers for inputs
in M. Structural induction on terms and bounded formulas proves this:
term values remain in M, and every bounded quantifier ranges over the same
finite set in both structures. Restricting the outer universal variables
of a true sentence preserves truth.

All intuitionistic logical schemes are valid in this Boolean structure,
and all its inference rules preserve that validity. Since FALSE is false,
some nonlogical axiom used in p must be false in M. The preceding paragraphs
leave only H.

Consequently M contains a counterexample (a,q) to H with q <= 2^k. Every
constant actually appearing in H has its standard interpretation in M,
because k >= r. The matrix of H is bounded, so the same absoluteness lemma
shows that (a,q) is also a genuine standard counterexample. By the faithful
meaning of Prf_I, q is an accepted I-proof of FALSE.

But

```text
q <= 2^k
  <= 2^(r+L)
   < 2^floor(log_2 p)
  <= p.
```

This contradicts minimality of p. There is no accepted I-proof of FALSE.
Thus H is true in the standard natural numbers, and every axiom of I is
true there. Ordinary soundness of intuitionistic deduction then gives full
external standard-model soundness of I as well.

This establishes the two SJAS requirements for the selected checker:

```text
I proves its own no-FALSE-proof assertion H;
I is actually consistent.
```

It establishes ordinary consistency, not Willard's stronger technical
Level(1) paired-proof assertion. No equivalence of those internal statements
is assumed.

## 6. Computational interpretation of the axioms

Attaching opaque proof constants to B and H would not suffice for the
requested computational interpretation. Instead use effective realizers.

All numerical primitives are computable. Every bounded formula D is
decidable on standard numerical inputs. By induction on D, prove both:

```text
D is realizable iff D is true;
if D is true, a realizer can be computed uniformly from D and its inputs.
```

The construction is explicit:

- True atomic relations have trivial evidence; false atoms have no realizer.
- Conjunction constructs a pair of recursively computed realizers.
- Disjunction decides which disjunct is true and constructs its tag and
  corresponding realizer.
- Bounded existential quantification searches the finite bound, then returns
  the witness and its recursively computed evidence.
- Bounded universal quantification gives a procedure for inputs in the bound;
  the bounded-quantifier guard handles inputs outside it.
- For a bounded implication D -> E, decide D. If D is false, its realizer
  domain is empty. If D is true and the implication holds, compute an
  E-realizer and return it on every D-realizer. The same induction shows
  that a false bounded implication has no realizer.

These are ordinary executable procedures. Runtime finite search is used in
the external extracted program; it is not added as an unbounded induction
or recursion axiom to I.

Each true axiom `forall inputs. D(inputs)` therefore has a computable
realizer: accept the inputs and run the bounded evidence constructor.
This covers F and every individually supplied naming axiom.

It also covers H, whose matrix is bounded and whose truth was proved in
Section 5. In this negative case there is no genuine realizer of the
counterexample predicate. Its rejecting implication consequently has an
empty realizer domain. No oracle deciding global consistency is required.
This does not mean arbitrary invalid inputs must satisfy the claimed proof
contract, or that H supplies unrestricted truth reflection.

### Explicit arithmetic realizer assignments

The follow-up question asks what these realizers actually are. Fix the
number-realizability convention in Sorensen-Urzyczyn, Definition 11.5.1:
the number 0 realizes a true atomic formula; no number realizes a false
atomic formula or FALSE. Use `(r,s)` for an effective numerical pairing,
`Left(r)` and `Right(s)` for tagged alternatives, and lambda notation for
indices of computable procedures. In particular, lambda notation below
describes external programs, not an additional term calculus inside I.

For the concrete finite F of Section 2, the following are realizers. Every
free numerical variable in the left column is universally quantified;
the corresponding lambda abstractions occur on the right. `h` denotes an
input realizer for an implication's antecedent.

| Arithmetic axiom matrix | Realizer of its universal closure |
| --- | --- |
| `Predecessor(x) = Subtract(x,1)` | `lambda x. 0` |
| `Subtract(x,0) = x` | `lambda x. 0` |
| `Subtract(x,x) = 0` | `lambda x. 0` |
| `Divide(x,1) = x` | `lambda x. 0` |
| `Divide(x,2) <= x` | `lambda x. 0` |
| `x <= Maximum(x,y) and y <= Maximum(x,y)` | `lambda x,y. (0,0)` |
| `Maximum(x,y) = Maximum(y,x)` | `lambda x,y. 0` |
| `(x <= y and y <= z) -> x <= z` | `lambda x,y,z. lambda h. 0` |
| `x <= y or y <= x` | `lambda x,y. if x <= y then Left(0) else Right(0)` |
| `not (0 = 1)` | `lambda h. 0` |

The constant programs are correct because the atomic conclusions are true
in the standard arithmetic interpretation. Returning 0 does not itself
verify or prove an arbitrary atomic assertion. For transitivity, a valid
antecedent realizer ensures the two inequalities, hence the conclusion.
For `not (0 = 1)`, there is no valid antecedent realizer, so the implication
clause has no inputs on which it demands a FALSE-realizer.

The naming axioms are explicit too. Willard 2006, Section 3, printed page
14, defines

```text
Add(x,y,z) := (x <= z) and (Subtract(z,x) = y).
```

Consequently each naming axiom `Add(C_i,C_i,C_(i+1))` is realized by `(0,0)`:
both its atomic conjuncts are true under `C_i = 2^(i-1)`. For the initial
constant-definition axiom, Equation (11), take the correspondingly nested
tuple of `0`, `lambda h. 0`, `0`, and `0`. These components realize its
conjuncts in formula order: the disequality `C_1 != C_0` is second, and the
other three conjuncts are equalities. These are realizers of each named
instance; they do not assert a
uniform arithmetic operation producing a successor or sum.

For a richer admissible F, define a total external constructor `build(D,n)`
which returns a realizer whenever bounded D is true at n, and a dummy value
when it is false. Decide D by finite evaluation, then, in the true case,
recurse on its syntax: pair conjunction evidence, select a true disjunct,
and search a bounded existential for a witness. For a true implication
`A -> E`, return `lambda h. 0` if A is false; otherwise return a constant
procedure producing `build(E,n)`. Bounded quantifiers retain their guards:

```text
exists k <= t. D(k):
  choose the least k <= t with D(k) true;
  return (k, (0, build(D,k))).

forall k <= t. D(k):
  return lambda k. lambda h.
    if k <= t then build(D,k) else 0.
```

Here the first formula expands as `exists k. (k <= t and D(k))`, and the
second as `forall k. (k <= t -> D(k))`; other numerical parameters are
implicitly passed through. Thus any admissible axiom `forall n. D(n)` has
realizer `lambda n. build(D,n)`. Only its bounded instances are decided at
runtime. Its truth for all n is an external hypothesis on the selected
axiom family, not a question that the constructor tries to decide.

Finally, write H as `forall a,p. (Bad(a,p) -> FALSE)`, where
`Bad(a,p) := FalseCode(a) and Prf_I(a,p)` is bounded. A realizer is simply

```text
r_H = lambda a,p. lambda h. 0.
```

Section 5 establishes that Bad is always false, and bounded-formula
realizability therefore gives no possible valid input h. This justifies
r_H by the implication clause. Its correctness depends on that external
consistency argument: writing this constant program provides no independent
consistency proof, no FALSE witness, and no internally checked inhabitant
of a dependent function type merely by its syntax.

Arithmetic computation still occurs in evaluating numerical terms and
constructing disjunction tags or existential witnesses. For Section 7's
division example, the extracted program can return
`lambda n. (Divide(n,2), (0,0))`: the witness performs the division, while
the two dummy components realize the true equality and bound.

### Why a constant realizer can carry almost no information

The user asks why `lambda x. 0` should signify anything. The program itself
carries almost no information. The significant assertion is the external
relation between that program and a particular formula. For an atomic P,
the chosen definition says

```text
Realizers(P) = {0}, if P is true;
Realizers(P) = {},  if P is false.
```

The value 0 is a placeholder, like a unit value. It does not encode an
arithmetic derivation or a Boolean answer to a truth test. A universal
realizer must terminate on each numerical input and return an element of
the corresponding instance's realizer set. Consequently, for atomic P(x),

```text
(lambda x. 0) realizes forall x. P(x)
    iff
forall x. P(x) is true in the standard interpretation.
```

For example the constant program realizes `forall x. Subtract(x,x) = 0`,
but fails to realize `forall x. Subtract(x,x) = 1`. Evaluating the program
does not distinguish these claims. Establishing its validity requires
mathematical reasoning about the specified formula. Realizer verification
for arbitrary formulas is not made decidable by this interpretation.

Computational information survives where the formula requires a choice or
a witness. A realizer of `A or B` must select a side and supply its realizer;
one of `exists y. R(y)` must supply a witness y; one of
`forall x. exists y. R(x,y)` must compute such witnesses from x. Thus
`lambda n. (Divide(n,2), 0)` realizes
`forall n. exists m. m = Divide(n,2)` and contains a nonconstant numerical
output. The full source derivation establishes why the extracted output is
correct, while the realizer can discard much of that derivation.

This is a semantic computational interpretation, not a supplied typing
derivation for the displayed lambda program. A full Curry-Howard proof term
can retain justification that this realizer interpretation discards. In
particular, r_H's very simple code does not reduce the work of establishing
consistency: its validity rests on the external argument in Section 5.
The construction's extraction claim is the effective passage from accepted
source proofs to programs satisfying the realizability relation. A native
dependent type checker certifying these programs has not been supplied.

## 7. Extraction theorem

For each accepted I-proof pi of A, compute a Kleene realizer E(pi) of A.
The proof is by induction on the finite intuitionistic derivation, using
Section 6 for its nonlogical axiom lines. Logical schemes have their usual
computable realizers; modus ponens applies the implication realizer to the
antecedent realizer; generalization abstracts over a numerical input.
Quantifier side conditions are exactly those checked by the calculus.

For example, the first two propositional schemes have the proof programs

```text
K = lambda a. lambda b. a
S = lambda f. lambda g. lambda a. f(a)(g(a)).
```

Disjunction uses tagged alternatives, conjunction uses pairs, and an
existential realizer contains a numerical witness with evidence. These
pairing and evaluation operations belong to the external computational
interpretation; their uniform arithmetic totality is not postulated in I.

In particular, if pi proves

```text
forall n. exists m. R(n,m),
```

where R is bounded arithmetic, then E(pi), applied to n, terminates with
a witness m and an R(n,m)-realizer. Projecting m gives a computable function
f, total on all standard natural-number inputs, with

```text
R(n, f(n)) true for every n.
```

Termination on the specified inputs is part of the universal/implication
realizability clauses. Truth of the bounded output predicate follows from
the equivalence proved in Section 6. This supplies a witness extraction
theorem, rather than merely a suggestive type notation.

A simple example, using the explicit finite F above, is the intuitionistic
proof of

```text
forall n. exists m. (m = Divide(n,2) and m <= n).
```

Equality reflexivity supplies the equality for witness Divide(n,2); the
division-bound axiom supplies the inequality. Conjunction introduction,
existential introduction, and universal generalization finish the proof.
The extracted witness function is `lambda n. Divide(n,2)`, accompanied by
computable evidence for its bound. More substantial bounded specifications
can use the computational realizers of the richer grounding axioms.

The same construction extracts disjunction tags and realizers. It does not
by itself prove a syntactic disjunction property saying that one disjunct
must have a formal I-proof; that would be an additional theorem.

## 8. Scope, computational limits, and evidence

This is a constructive logic with a computational interpretation of every
axiom, including its own consistency assertion. The consistency proof is
external and may use classical metamathematics, as permitted by the user's
clarification. The proof predicate names the full intuitionistic theory,
including H; it is neither a predecessor checker nor a separate classical
certificate requirement.

Its weak native arithmetic is an explicit tradeoff. This construction does
not grant unrestricted natural-number induction, a primitive recursor, or
uniform successor/addition/multiplication totality in the native arithmetic.
Higher-order proof functions and external program operations must be kept
distinct from those internal arithmetic assertions. Adding Heyting
arithmetic's usual strength would restore the G2 obstruction.

The earlier
[constructive-fragment and paired-certificate note](2026-09-06-constructive-sjas-and-tableaux.md)
remains valid as a different construction. The present argument addresses
its previously open requirement that the new axiom directly concern the
constructive checker's own proofs. It does not settle the stronger,
addition-total, native intuitionistic-tableaux variant.

| Component | Evidence and status |
| --- | --- |
| Non-growth language and named-number method | Willard 2006, Sections 1, 3, 4; imported definitions. |
| Bounded syntax, proof checking, and self-substitution | Willard 2006, Lemma 1 is a sketch with a reference to Willard 2001, Appendices B-D; this imported arithmetization supports the specified syntactic restriction and coding. |
| Consistency of the intuitionistic construction | Complete paper argument in Section 5, adapting the published finite-model argument and making the coding inequalities explicit. |
| Computational interpretations of added axioms | Bounded-formula induction in Section 6, using the externally proved truth of H. |
| Witness extraction | Realizability induction in Section 7; the logical interpretation is standard, the axiom interpretation is supplied here. |
| Executable checker, extraction implementation, mechanization | Not implemented or claimed. |

Completion audit for the mathematical design:

- Requirement 1 is witnessed by the actual intuitionistic schemes and rules
  in Section 3, together with the axiom realizers in Section 6. The explicit
  finite F in Section 2 supplies a concrete admissible instance.
- Requirement 2 uses the displayed bounded reduction to the source's
  ExtraPrf predicate and the self-substitution construction in Section 4.
  Every accepted record has one intuitionistic derivation; its extra axiom
  is the very H whose proof predicate is being defined.
- Requirement 3 is the axiom proof of H plus the strict decreasing-proof
  contradiction in Section 5. The size estimate counts the expanded H and
  actual stored name indices, not abbreviated predicates or macro names.
- Requirement 4 is the bounded-realizer construction and proof induction in
  Sections 6-7. The division example explicitly yields a witness and bound
  evidence. No claim that every true statement is provable, or that every
  computable function has a totality proof, is needed for extraction.

This audit establishes the mathematical construction relative to the cited
arithmetization and ordinary metatheory. It is not an audit of executable
software or a machine-checked proof artifact.

The relevant source pages were checked as rendered images: Willard 2006,
printed/PDF pages 16-21, and Sorensen-Urzyczyn, printed pages 176-178 / PDF
pages 188-190. Whitespace and local-link checks are documentation validation,
not a mechanized proof of the metatheory.

Sources:

- Dan E. Willard, *A Generalization of the Second Incompleteness Theorem and
  Some Exceptions to It*, APAL 141 (2006), 472-496,
  [DOI](https://doi.org/10.1016/j.apal.2005.12.010),
  [local author version](../../nachlass/papers/willard2006_generalization_second_incompleteness_author_apal6.pdf).
- Dan E. Willard, *A Detailed Examination of Methods for Unifying,
  Simplifying and Extending Several Results About Self-Justifying Logics*,
  [2011 manuscript](https://arxiv.org/abs/1108.6330), Definition 3.4,
  Theorem 5.9 and Appendix D-3. The Hilbert stability discussion there is an
  alternative route; the consistency proof above uses the fuller 2006
  finite-model argument instead of treating its stability sketch as a
  mechanized result.
- Morten Heine Sorensen and Pawel Urzyczyn, *Lectures on the Curry-Howard
  Isomorphism*, Sections 5.3 and 11.5,
  [author lecture notes](https://kwarc.info/teaching/sWuV/sorensen-urzyczyn_lectures-on-the-curry-howard-isomorphism.pdf).
