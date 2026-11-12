# SJAS program typing: bounded witnesses and representation

Date: 2026-09-22 (America/Indiana/Indianapolis).

## Recovered question and scope

The question was: "What programs can be typed using an SJAS as the corresponding
logic?" The subsequent instruction was: "Do not regurgitate what is in this repo,
reason thoroughly, from first principles and the literature, to answer the
question." The present request was "continue".

This note continues the argument with an exact result about a specified family
of numerical specifications, followed by consequences for data representation
and explicit execution certificates. It does not posit a new type checker,
change an ADR, or establish a Curry-Howard translation for a new deductive
apparatus. All mathematical arguments below are paper proofs, not mechanized
results.

The checkpoint was recovered from session
`01a0c7c7-7ffe-7d40-b803-f042255775fc`, whose final answer distinguished ordinary
logical proof terms, native arithmetic specifications, and certified reflection.
The new derivations use the primary source below and their explicit hypotheses;
repository proposals are not premises.

## Definitions and imported theorem

Fix Willard's U-grounding language L*: constants 0, 1, 2; addition and doubling;
and the non-growth operations subtraction, division, root, maximum, logarithm,
and bit counting. Interpret these in the standard natural numbers, with the
conventions specified in Appendix D-1. Every term has a bound

    t(x_1,...,x_r) <= c_t (1 + max_i x_i)

for some standard constant c_t. Here multiplication by the fixed constant is
metanotation for finitely many additions, not an added multiplication symbol.

Write Delta*_0 for formulas whose quantifiers are all bounded by L* terms, and
Pi*_1 for universal closures of such formulas. Let T be any sound first-order
theory axiomatized entirely by true Pi*_1 sentences in this language. The upper
bound below applies to any sound deduction method for T; self-justification is
not one of its proof hypotheses.

For the converse, fix the EA-stable semantic-tableaux configuration xi* in
Willard's Definition D.1 and Theorem D.4. The imported fact is Theorem 6.3:
adjoining a recursively enumerable collection theta of true Pi*_1 sentences
and forming the specified self-consistency axiom for that enlarged theory
produces a consistent self-justifying system. The construction uses the
designated proof encoding and deduction apparatus.

Source: Dan E. Willard, *A Detailed Examination of Methods for Unifying,
Simplifying and Extending Several Results About Self-Justifying Logics* (2011),
[Lemma 6.1, Definition 6.2, and Theorem 6.3](https://arxiv.org/pdf/1108.6330#page=21),
[Appendix D-1](https://arxiv.org/pdf/1108.6330#page=38).
The source statements on PDF pages 21, 22, 38, and 39 were rendered and visually
checked in this continuation. Theorem 6.3 is used as an imported theorem; this
note does not independently re-prove its stability foundation.

## A bounded-witness lemma

**Claim.** If R(x_1,...,x_r,y) is Delta*_0 and

    T proves forall x_1 ... forall x_r exists y R(x_1,...,x_r,y),

then there is a standard positive integer c such that

    N satisfies forall x_1 ... forall x_r
                exists y <= c(1 + max_i x_i) R(x_1,...,x_r,y).

The conclusion is a standard-model bound. This argument does not assert that T
itself proves the bounded version, or that one can effectively extract c from a
proof by the argument as given. R need not define a unique result.

**Proof.** Suppose no such c exists. Expand the complete theory of the standard
L* structure by constants a_1,...,a_r and, for each positive standard integer k,
the sentence

    forall y <= k(1 + max_i a_i) not R(a_1,...,a_r,y).

Every finite subset is satisfiable in the standard structure: choose inputs
violating the largest of the finitely many bounds. Compactness gives a model M
of all these sentences and the complete theory of the standard structure.
Put m = 1 + max_i a_i and form the external initial segment

    I = { b in M : b <= k m for some positive standard integer k }.

It contains each a_i and every standard constant. If b <= k m and d <= l m,
then b+d <= (k+l)m, so I is closed under addition and doubling. Non-growth
operations also preserve I. Thus I is an L* substructure of M.

An induction on formulas shows Delta*_0 absoluteness between I and M. Atomic
formulas agree; Boolean connectives preserve agreement; every bounded
quantifier with parameters in I ranges over the same elements in both
structures, since its bound is the value of an L* term in I and I is initial.

Every true Pi*_1 axiom of T holds in M and hence in I. Consequently I is a model
of T. Soundness then requires a witness y in I to R(a_1,...,a_r,y). But every
element of I is below one of the bounds forbidden in M, and absoluteness
transfers that prohibition to I. Contradiction.

This strengthens the earlier presentation by making explicit that the witness
lemma applies to relations, not just uniquely defined functions.

## Exact characterization across this additive family

Fix a Delta*_0 formula R defining the graph of a standard total numerical
function f. Then the following are equivalent:

1. Some self-justifying system obtained from xi* by a true, recursively
   enumerable Pi*_1 extension and the corresponding self-consistency
   construction proves totality and uniqueness for R.
2. For some standard constant c,
   f(x_1,...,x_r) <= c(1 + max_i x_i) for all standard inputs.

For (1) implies (2), apply the bounded-witness lemma and use uniqueness in the
standard model. The systems in question have true Pi*_1 axioms, including their
true self-consistency assertion.

For (2) implies (1), take theta to contain the two true sentences

    forall x_1 ... forall x_r
      exists y <= c(1 + max_i x_i) R(x_1,...,x_r,y)

and

    forall x_1 ... forall x_r forall y forall z
      ((R(x_1,...,x_r,y) and R(x_1,...,x_r,z)) implies y=z).

Both are Pi*_1, and theta is finite. Theorem 6.3 supplies the desired
self-justifying extension. Its first added axiom implies unbounded totality and
its second supplies uniqueness.

This equivalence is a deduction here from the witness lemma and Willard's
theorem. It is not an attributed quotation or named theorem from the paper.
The quantifier is **some theory in the family**. It does not say that every
fixed SJAS proves every true bounded specification. It also does not license
adding axioms to a fixed system while keeping its former self-consistency
sentence unchanged. The consistency assertion must refer to the enlarged
system as in the cited construction.

## Concrete numerical algorithms

For each fixed bounded graph and known bound b, the canonical witness algorithm
is finite search through 0,...,b, evaluating the bounded formula on each
candidate. All primitives are computable and every quantifier is finite.
Truth of the bounded existence assertion proves externally that this search
finds a result. This supplies a particular computable realizer; the imported
classical theorem itself is not a program-extraction theorem.

Examples whose correct bounded specifications can be accommodated include:

| Result | Numerical bound | Witness algorithm |
| --- | --- | --- |
| Primality Boolean | 1 | Check bounded possible factors |
| Least divisor at least 2, with result 0 for inputs below 2 | n | Search candidate divisors in increasing order |
| Greatest common divisor, taking gcd(0,0)=0 | max(a,b) | Search common divisors and choose the largest |
| Witness to a fixed bounded relation, or a failure tag | the supplied bound for the witness | Exhaust the bounded range |

For clarity, these graph claims need no multiplication term. Willard's
Equation (45) gives a bounded formula Mult(d,q,n). Thus divisibility can be
written as `exists q <= n Mult(d,q,n)`, and leastness, primality, and greatest
common-divisor conditions use only further bounded quantification. The gcd
definition bounds both its proposed result and compared divisors by max(a,b),
which also handles (0,0) as specified. A tagged search result uses two output
components, each bounded, rather than silently packing them into one integer.

An unrestricted native squaring function is excluded because its unique
output n^2 has no such linear bound. An algorithm that searches for a product
only up to an input bound and reports failure if none exists has a different,
bounded specification. This distinction depends on the specified output
contract, not on whether multiplication occurs informally in the algorithm's
description.

## Pair construction versus coding two numbers into one

Suppose a bounded graph P(x,y,z) defines a total injective pairing function and
T proves its totality. The witness lemma gives

    pair(x,y) <= c(1 + max(x,y)).

For inputs x,y <= N there are (N+1)^2 distinct pairs but at most c(N+1)+1
possible outputs. For sufficiently large N, injectivity is impossible. Thus
no bounded graph of an injective native pairing function can have its totality
proved in this family.

This does not obstruct logical products. With ordinary product and contraction
rules, `lambda x. (x,x)` has type `Num -> Num x Num`. Its two components need
not be packed into one native arithmetic object. An encoder from an arbitrary
pair to one native number is an additional operation, with the obstruction
just proved. No claim is made here that the source first-order object language
already has higher-order function sorts.

Equivalently, a certified unique numerical output has binary length at most
the largest input's binary length plus a constant depending on the program.
Two independently varying numbers generally require the sum of their lengths
for an injective encoding. This is why ordinary products and native coded
products have different obligations.

## Explicit execution certificates impose another condition

Suppose Run_e(x,p) is a Delta*_0 predicate for a fixed program e, and p is a
native number encoding a complete explicit execution trace. If

    T proves forall x exists p Run_e(x,p),

the same witness lemma forces some accepted trace code p <= c(x+1) for every
standard x. Under an encoding with at least one bit per transition and a
length marker, that gives an O(log(x+1)) bound on the number of explicitly
listed transitions in such a trace.

For example, a program which makes at least x transitions and then returns 0
has the trivially bounded output graph y=0. But the preceding hypotheses
prevent this family from proving the existence of a complete ordinarily
encoded trace for every input x: such a trace would eventually exceed the
linear code bound. The statement is conditional on the specified bounded
checker and explicit representation. It makes no claim about compressed
certificates or another representation.

A Curry-Howard typing derivation does not in general require a trace of runtime
execution. This example distinguishes two different contracts; it does not
impose a correspondence between individual proof steps and runtime events.
Likewise the canonical bounded-search realizer above does not come with an
automatic internal theorem asserting that all of its full trace codes exist.

## What is and is not established about typing

The source theorem establishes self-justifying arithmetic theories proving the
specified formulas. The arguments here supply numerical obstructions and
explicit external witness algorithms. To identify those algorithms with
inhabitants of a particular dependent type such as

    (x : Num) -> Sigma (y : Num). R(x,y),

one must give the chosen constructive or classical computational
interpretation, the interpretation of nonlogical axioms, and its proof or
typing preservation result. A lambda type checker added to a host language is
not automatically the proof apparatus mentioned by the source consistency
sentence. Those obligations remain separate from the completed paper proofs
above.

No implementation, checker self-consistency theorem, runtime complexity
classification, or mechanized verification is claimed by this note.
