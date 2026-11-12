# Constructive SJAS and the tableaux variants

Date: 2026-09-06 (America/Indiana/Indianapolis).

This record preserves the preceding question and response, followed by the
requested tableaux follow-up. Formulas use CLI-readable ASCII. The original
response is preserved below, excluding conversation memory metadata. These are
paper constructions, not claims of a completed checker implementation or
mechanized metatheory.

## Original question

> Can a constructive Willard-style SJAS be built, such that the system has a clear computational correspondent?

## Original response

Yes. There is a concrete route to an intuitionistic SJAS with computable proof realizers: take an appropriately chosen intuitionistic fragment of a Hilbert-self-justifying Willard theory.

This is a construction from Willard's results and standard computational interpretations of intuitionistic logic. It also has an important qualification: the inherited self-consistency axiom refers to the classical system; the constructive system derives a consistency theorem about its own checker.

Start with a consistent Willard theory W whose nonlogical axiom list contains its self-consistency sentence H_W. Willard's ISCE construction supplies Hilbert-style examples under its stated soundness and encoding hypotheses. Its arithmetic is sufficiently weak to permit self-justification. [Willard 2006, Theorem 3 and Section 4](https://doi.org/10.1016/j.apal.2005.12.010)

Choose the classical Hilbert presentation, before forming H_W, to consist of intuitionistic logical schemes together with additional classical schemes. Willard's later Hilbert configuration allows the choice of a conventional Hilbert presentation. [Willard 2011, Appendix D-3](https://arxiv.org/pdf/1108.6330#page=47)

Now define I:

    Nonlogical axioms: exactly those of W.

    Logical inference: intuitionistic Hilbert logic.

    Proof records: the same format used by W,
                   restricted to intuitionistic logical steps.

Make the restriction explicit in its arithmetic proof predicate:

    Prf_I(a,p) :=
        Prf_W(a,p) and IntOnly(p)

Here a codes the conclusion, p is the proof record, and IntOnly checks the logical rule tags and their side conditions. It examines the derivation independently of whether its conclusion is true or false.

Consequently, intuitionistic logic alone proves:

    I |- Prf_I(a,p) -> Prf_W(a,p)

This inclusion needs no proof expansion or normalization.

We can now establish both parts of self-justification.

First, I is externally consistent. Every I-proof is already a W-proof. An I-refutation would therefore contradict W's externally established consistency.

Second, I proves its own consistency. Write W's inherited axiom in curried notation:

    H_W :=
        forall a,p.
        FalseCode(a) -> Prf_W(a,p) -> FALSE

FalseCode retains the source's predicate identifying the code of its chosen contradiction, 0=1. This represents the Group-3 no-refutation assertion.

Inside I, reason as follows:

    Assume FalseCode(a).
    Assume Prf_I(a,p).

    Obtain Prf_W(a,p) by conjunction elimination.

    Apply H_W.

    Obtain FALSE.

Discharging the assumptions gives:

    I |- forall a,p.
         FalseCode(a) -> Prf_I(a,p) -> FALSE

That is I's own no-refutation statement, concerning proofs from its complete axiom list, including H_W.

The distinction is precise:

    H_W is an inherited axiom about W.

    H_I is a derived theorem about I itself.

Thus I satisfies Willard's two requirements: it proves the specified consistency statement for its own deduction system, and it is externally consistent.

There is also a direct computational interpretation.

Intuitionistic Hilbert proofs correspond to typed combinator programs. For example:

    Logical axiom                  Program constant

    A -> B -> A                    K

    (A -> B -> C)
      -> (A -> B) -> A -> C         S

    Modus ponens                   Application

Their computation rules are:

    K x y     --> x

    S f g x   --> f x (g x)

First-order quantification adds the usual individual parameters and dependent typing structure. The propositional correspondence is given explicitly in [Sorensen and Urzyczyn, Proposition 5.3.8](https://kwarc.info/teaching/sWuV/sorensen-urzyczyn_lectures-on-the-curry-howard-isomorphism.pdf#page=95).

The consistency derivation itself has this proof term, written using lambda notation:

    self_I a p falsity accepted =
        self_W a p falsity (first accepted)

Here:

    p          is a proof code.

    accepted   is evidence for
               Prf_W(a,p) and IntOnly(p).

    first      extracts the evidence for Prf_W(a,p).

    self_W     is the inherited axiom's proof constant.

This gives a concrete proof-program correspondence. The native certificates can remain the fixed Hilbert records, interpreted as typed combinator graphs.

But constructive inference alone would not establish useful witness extraction: the nonlogical axioms also need computational interpretations. We can address that for the selected Willard systems.

Choose the arithmetic axioms in their true Pi_1 form:

    forall inputs. D(inputs)

where D is bounded and effectively decidable. Willard's relevant axiom groups have this form; the self-consistency axiom is also universal with a bounded checking matrix.

For each such axiom, construct a realizer as follows:

1. Accept the numerical inputs.
2. Evaluate the bounded matrix.
3. Construct its evidence, using finite search for bounded existential witnesses and the corresponding operations for the other connectives.

An induction on bounded formulas establishes this construction. Because the axiom is true for every input, the procedure supplies appropriate evidence for every input. The same reasoning applies to H_W once Willard's external consistency theorem establishes its truth.

Intuitionistic inference composes these realizers. This is an application of [Kleene realizability](https://kwarc.info/teaching/sWuV/sorensen-urzyczyn_lectures-on-the-curry-howard-isomorphism.pdf#page=188). In particular, from an I-proof of

    forall n. exists m. R(n,m)

with a decidable arithmetic relation R, we can extract a computable function f satisfying:

    R(n, f(n))

for every standard input n. That provides computational content beyond merely attaching uninterpreted constants to proofs.

The G2 boundary remains essential. Our earlier G2 argument already works intuitionistically. Therefore this construction must retain the weak arithmetic and proof representation that prevent the full internal G2 package from holding. The typing rule for application composes proofs; it does not automatically supply an internal theorem asserting totality of the corresponding numerical proof-code operations.

This establishes a paper-level construction of a constructive descendant of a Willard system. A fully specified implementation and machine-checked adequacy proof remain separate work. A stronger requirement that the added axiom directly name only I's own checker, rather than deriving H_I from H_W, would also require a further construction.

## Follow-up request

> Log this. What about for the linguistically more expressive variants of SJAS based on tableau deduction?

## Follow-up response: constructive derivations with source tableaux

Yes: the richer Type-A tableaux systems admit a constructive descendant with
an explicit computational interpretation. A sufficient certificate format
contains both a constructive derivation and a complete tableau checked by the
original source system. The consistency transfer below is a mathematical
construction from Willard's source theorem, not a construction attributed to
Willard himself.

The distinction from the preceding Hilbert proposal matters. Classical semantic
tableaux do not become an intuitionistic calculus merely by deleting a rule
labelled excluded middle. Their other rules already support classical reasoning.
Instead, the constructive component must be checked by a specified intuitionistic
calculus.

### Source and language

Fix a source W from Willard's configuration xi*, with semantic tableaux as its
deduction method. Fix its language, base axioms, deduction rules, and proof
encoding before forming its self-consistency axiom H_W. Retain the resulting
full source proof predicate unchanged.
Theorem D.4 establishes the stability hypothesis used by Theorem 5.9 to establish
external consistency of the self-justifying source.
[Willard 2011, Theorems 5.9 and D.4](https://arxiv.org/pdf/1108.6330#page=44)

This language includes addition and doubling as total function symbols, together
with truncated subtraction, division, roots, maximum, logarithm, and bit counting.
Multiplication has a definable graph, Mult(x,y,z), without a multiplication
function symbol or an assertion of its unbounded totality. This is a richer
arithmetic term language than the Hilbert configuration xi-.
[Willard 2011, Appendix D-1](https://arxiv.org/pdf/1108.6330#page=38)

Choose the source base from the effective true Pi_1* axiom sets allowed by the
source construction. Its inherited H_W is also true by the external consistency
theorem. This truth assumption will be used for the computational interpretation;
consistency alone would not establish realizability of arbitrary extra axioms.

### Exact certificate interface

Let J be intuitionistic first-order natural deduction with equality, over W's
exact nonlogical axiom list, including H_W. Use explicit proof terms and typing
derivations, with the usual binding and eigenvariable conditions. The logical
rules are the usual rules for FALSE, and, or, implies,
forall, and exists, together with equality reflexivity and substitution. No
additional induction or recursion rule is introduced.

Write:

    Tab_W(a,p) =
        p is an actual source tableau proving the sentence coded by a.

    Int_W(a,r) =
        r records a closed J-derivation of that same sentence.

The constructive record must contain the derivation and any finite verification
trace needed for its arithmetic checking predicate. An implementation must use a
faithful formula in the source language for Int_W, rather than an uninterpreted
new predicate. Fully annotated finite derivations allow checking by a numerical
computation trace, including axiom-enumeration evidence if the axiom set is only
recursively enumerable. Trace witnesses belong to the submitted record; their existence
is not asserted by a new internal totality theorem. If several numerical fields
are needed, keep them as several arguments. No source-provable total pairing
operation is assumed.

The following transfer argument works for any such faithful Int_W formula. In
particular, choosing a bounded trace-checking formula keeps the resulting
consistency statement in the source's universal bounded-matrix format. This note
specifies the interface and proves the transfer; it does not provide a concrete
bit-level definition of that checker.

Define the deduction system C by the same nonlogical axioms as W and the
certificate acceptance predicate:

    K(a,p,r) := Tab_W(a,p) and Int_W(a,r).

Here K is a formula abbreviation, expanded in the original language. An accepted
certificate is the tuple (p,r). The original source tableau p is required in full;
a request to expand a proof macro is not a substitute for p.

Externally, C has exactly J's theorems:

    C proves A  iff  J proves A.

The forward direction projects r. For the reverse direction, a J-proof is also
a classical consequence of the same nonlogical axioms, so completeness of the
fixed source tableaux supplies p. With an effective source, enumerating tableaux
until that p is found gives an external algorithm. This is theorem-set equivalence
and an external proof transformation; it is not an internally certified
transformation of numerical proof codes.

### Both parts of self-justification

First, C is externally consistent. Any C-proof of a contradiction contains a
W-tableau of the same contradiction, contrary to the source consistency theorem.

Second, C proves the following consistency statement for its own full checker.
Retain the exact Level(1) convention of the source:

    Neg1(a,b) =
        a codes a Pi_1* sentence and b codes its formal Sigma_1* negation.

The inherited axiom has the form:

    H_W :=
        forall a,b,p,q.
        not (Neg1(a,b) and Tab_W(a,p) and Tab_W(b,q)).

The statement about C's own checker is:

    H_C :=
        forall a,b,p,q,r,s.
        not (Neg1(a,b) and K(a,p,r) and K(b,q,s)).

Neg1 retains the source's syntactic classes and formal-negation operation.
Willard's Level(1) statement is pairwise consistency for this class; it should not
be identified merely with the assertion that there is no proof of 0=1.
[Willard 2011, Definitions 5.6-5.7, Remark 5.8, and the formula in Theorem 5.9](https://arxiv.org/pdf/1108.6330#page=18)

There is an intuitionistic derivation r_H of H_C. Given the three conjuncts of a
purported counterexample to H_C, project Tab_W(a,p) from K(a,p,r) and Tab_W(b,q)
from K(b,q,s). Together with Neg1(a,b), these contradict H_W. Discharge the
assumption and universally generalize the six numerical parameters.

Using a curried version of H_W, its proof term is:

    separate_C a b p q r s n kp kq =
        separate_W a b p q n (first kp) (first kq)

Here n is evidence for Neg1(a,b); kp and kq are evidence for the two K predicates;
and separate_W is the proof constant for the inherited axiom. Numerical codes,
evidence about checker acceptance, and the constructive program remain distinct.

We must also supply a source tableau p_H of H_C, since r_H alone is not a C-proof.
There is an explicit finite analytic derivation:

1. Start with not H_C. Expand its negated universal quantifiers, introducing
   fresh parameters a,b,p,q,r,s. Double-negation elimination and conjunction
   expansion yield Neg1(a,b), K(a,p,r), and K(b,q,s).
2. Expand the two K abbreviations and their conjunctions. The branch now contains
   Neg1(a,b), Tab_W(a,p), and Tab_W(b,q), as well as the two Int_W formulas.
3. Write the proper axiom H_W on the branch and instantiate its four universal
   quantifiers with a,b,p,q. Its matrix is the negation of the conjunction of the
   three formulas already present.
4. Apply the ordinary negated-conjunction splitting rules. Every resulting branch
   contains a formula and its negation, and therefore closes.

Schematically, writing N, P, Q for those three formulas, the last step is:

    branch facts: N, P, Q
    added instance: not (N and (P and Q))

                    /                 \
                 not N          not (P and Q)
                 closed           /       \
                               not P     not Q
                               closed    closed

This tree uses only the source's ordinary connective, quantifier, proper-axiom,
and closure rules. The Int_W subformulas need not be analyzed. Any required
syntactic preprocessing belongs to the fixed source presentation. In particular,
the argument does not introduce an unrestricted cut rule or treat an arbitrary
derived theorem as a proper axiom.

If a selected presentation allows closure only on complementary literals,
expand each displayed complementary formula pair into its finite identity
tableau. These are fixed formulas; this step asserts no uniform internally
certified procedure on arbitrary numerical proof codes.

Consequently, (p_H,r_H) is an accepted C-certificate of H_C. Both requirements
have been established: external consistency and a theorem concerning C's own
deduction method. H_W remains the inherited axiom; H_C is a derived theorem
about the new checker. No fresh self-referential axiom is silently added.

### Computational interpretation

The r component provides the usual constructive proof-program correspondence:

    A -> B              function from evidence of A to evidence of B
    A and B             pair
    A or B              tagged alternative
    forall x. A(x)      function accepting a numerical input
    exists x. A(x)      witness together with evidence

For example, the richer arithmetic language directly supports:

    add  : Num -> Num -> Num
    dup  : Num -> Num
    dup x = add x x

This Num interface does not include an unrestricted natural-number recursion or
induction principle. First-order quantification over numbers should not be
confused with granting all eliminators of an inductive natural-number type in a
stronger host theory.

For actual computation, interpret the selected true universal bounded-matrix
axioms by the bounded decision and evidence-building procedures described in the
original response. The same interpretation works with the computable
U-grounding primitives. Composing these realizers through r yields, from an
accepted proof of

    forall n. exists m. R(n,m)

for bounded arithmetic R, a computable function f with R(n,f(n)) for every
standard input n. The tableau p supplies the source certificate; r supplies the
constructive evidence used for extraction. The realizability argument is external.
[Sorensen and Urzyczyn, Section 11.5](https://kwarc.info/teaching/sWuV/sorensen-urzyczyn_lectures-on-the-curry-howard-isomorphism.pdf#page=188)

### Where proof composition remains significant

At the level of J-derivations, ordinary application forms a proof of B from
proofs of A -> B and A. At the level of C-certificates, an accepted proof of B
also requires a source tableau of B. Producing this tableau can require substantial
proof expansion. The result above does not prove inside C the uniform bridge:

    forall a,r.
    Int_W(a,r) -> exists p. Tab_W(a,p).

Nor does it prove inside C that every pair of source tableaux has a suitably
encoded composed tableau. External completeness or cut elimination does not
supply those internal totality statements.

Thus H_C is not being identified with consistency of the checker that accepts r
alone, or with consistency of an arbitrary host dependent type theory. The
earlier [macro-enabled lowering obstruction](2026-09-05-lower-resolution.md)
is relevant precisely because removing the requirement to supply the source
tableau can change the internal consistency question. That obstruction concerns
its specified Kmac checker and does not settle every possible proof-term format.

For the source variant xi**, the same transfer applies with its own fixed
Tab-U*_1 proof predicate. That source already permits certain cuts on Pi_1* and
Sigma_1* sentences. Its stability is described in Appendix D-3 by reference to
the earlier construction, rather than reproved there in full. This permits some
source-level proof reuse, but it does not authorize arbitrary cuts or a different
checker while retaining the same self-consistency axiom.
[Willard 2011, Appendix D-3](https://arxiv.org/pdf/1108.6330#page=46)

### Status and evidence

The established result here is the consistency-transfer construction for the
specified certificate interface, including a constructive proof and an analytic
source proof of H_C. The original source theorem is imported; its hypotheses and
proof encoding are retained. Willard 2011 supplies full arguments for Theorems
5.9 and D.4; the xi** extension in Appendix D-3 is described by reference and
generalization. This note supplies no machine-checked proof, implemented Int_W
arithmetization, or checker benchmark.

A single native intuitionistic tableaux calculus whose program annotations alone
determine accepted source certificates would require an explicit rule system and
an adequacy proof. It is a stronger deliverable than the paired-certificate
construction established here. Likewise, adding an axiom that directly names only
the new checker requires a further self-reference and consistency argument.

The source pages carrying the language, Level(1) predicate, and stability theorem
were inspected as rendered PDF pages. This is a documentation and mathematical
logging change; no implementation or repository test-suite claim is made.
