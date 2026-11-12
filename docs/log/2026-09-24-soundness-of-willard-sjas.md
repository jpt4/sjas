# Soundness of the Willard self-justifying constructions

Date: 2026-09-24

The user asked:

> Are SJAS sound? They are incomplete, but are they shown to only prove true sentences?

For the Willard constructions under discussion, full standard-model soundness
follows externally under the stated construction hypotheses. The definition
of self-justification itself requires consistency and an internal assertion
of the selected consistency property; standard-model soundness uses the
additional structure of these constructions.

Here is an explicit general lemma underlying that answer. Fix a standard
arithmetic interpretation, a sound ordinary deduction method, and a base B
whose axioms are true and which proves every true Sigma_1 sentence of the
selected language. Let

```text
S = B + Gamma,
```

where every added axiom in Gamma is Pi_1. If S is consistent, then S is sound.

Proof:

1. Suppose an added axiom A is false in the standard model.
2. Its negation is a true Sigma_1 sentence, so B proves not A.
3. S has A as an axiom and also proves not A, contradicting consistency.
4. Thus every added axiom is true. All axioms of S are consequently true.
5. Soundness of the deduction method makes every S-theorem true, regardless
   of its quantifier complexity.

The contradiction in step 3 only needs the ordinary deductive closure of the
selected apparatus; it does not assume an internally certified uniform
proof-composition function. The lemma is an external paper proof.

Willard's 2011 framework provides its hypotheses: Definition 3.4(3) supplies
the true, Sigma_1-complete base; Definitions 5.7 and 6.2 make both the added
theta axioms and the self-consistency axiom Pi_1 in the native hierarchy.
Theorem 6.3 establishes consistency under the appropriate stability condition
when theta is true. Applying the lemma therefore establishes full soundness
for those instances with the usual sound deduction methods.
[Willard 2011, Definitions 3.4, 5.7, 6.2 and Theorem 6.3][willard]

Equivalently, the external consistency theorem makes the correctly encoded
self-consistency axiom true. Since the other axioms were already true, all
axioms are true and ordinary deductive soundness finishes the argument. This
does not presume the truth of the self-consistency axiom in order to justify
its addition; the external consistency theorem is the substantive input.

The base and imported axioms retain their stated truth assumptions. This is
not a theorem that arbitrary additional axioms preserve soundness or that
an arbitrary consistent theory proves only standard-model truths. In these
constructions a false Pi_1 input is refutable by B and makes the extension
inconsistent.

Soundness and incompleteness are compatible: every theorem can be true while
some true sentences remain unprovable. The soundness argument here is external;
it does not supply an unrestricted internal reflection principle identifying
proof acceptance with truth for arbitrary sentences.

For these concrete sound instances, this argument supplies the soundness
hypothesis in the earlier
[halting/theoremhood comparison](2026-09-23-two-computational-boundaries.md).
The comparison's general proposition appropriately keeps that hypothesis
explicit for other possible systems.

Verification: Definition 3.4 on printed page 9, Definition 5.7 on page 17,
Definition 6.2 on page 20, and Theorem 6.3 on page 21 were checked against
rendered PDF images. The source has a proof of Theorem 6.3 using its earlier
stability results; this turn did not independently replay every stability
proof. The soundness consequence is the complete paper derivation above,
conditional on the source theorem and the stated hypotheses, and is not a
mechanized result.

## Completeness, consistency, and intended-model truth

The user correctly observed that a syntactically complete, consistent theory
may select the false member of a sentence/negation pair in an intended
interpretation. The proposed additional condition that all true sentences are
provable needs a specified interpretation and a distinction from ordinary
semantic completeness of a logical calculus.

Fix a classical structure or valuation M. The three relevant directions are:

```text
Soundness for M:               T proves A  implies  A is true in M.
Completeness for M:            A is true in M  implies  T proves A.
Semantic completeness of logic:
  A is true in every model of T  implies  T proves A.
```

The last is the usual consequence version of the logical completeness theorem.
It concerns every model satisfying the axioms, and does not establish that a
designated intended interpretation satisfies those axioms. See Simpson,
*Mathematical Logic*, Definition 5.1.2(2), printed page 87, checked against a
rendered page image. [Source][simpson]

If T is consistent and complete for M, it is sound for M. Indeed, if T proved
A while A were false in M, then not A would be true in M. Completeness for M
would supply a T-proof of not A, contradicting consistency. Syntactic
completeness is unnecessary for this argument; completeness for M already
implies it by classical bivalence.

For a concrete counterexample to syntactic completeness plus consistency
implying intended truth, take classical propositional logic with just one atom
p and the additional axiom p. There is exactly one valuation satisfying that
axiom. Each formula or its negation follows by its truth table, and no formula
and its negation both follow. The theory is therefore syntactically complete
and consistent. Relative to the intended valuation assigning false to p,
however, its axiom p is false. The ordinary semantic completeness of the
propositional calculus remains intact.

Conversely, if T is syntactically complete and sound for M, then it is complete
for M: for any true A, the decision between A and not A cannot choose not A
without violating soundness. Thus for a consistent syntactically complete T,
soundness for M and completeness for M are equivalent.

Full completeness for M is not needed to establish soundness of theories in
general. True axioms and truth-preserving deduction suffice. In the Willard
argument above, the restricted Sigma_1 completeness is used to refute any
false Pi_1 added axiom; full completeness for arithmetic truth is not assumed.

[willard]: https://arxiv.org/pdf/1108.6330
[simpson]: https://people.math.wisc.edu/~awmille1/old/m571-08/simpson.pdf#page=88
