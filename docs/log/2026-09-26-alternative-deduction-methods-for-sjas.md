# Cirquents, deep inference, proof nets, and SJAS deduction

Date: 2026-09-26.

Question: Could cirquent calculus, other deep-inference methods, proof nets,
or interaction networks serve as deduction methods for an SJAS? How would
they be incorporated, and what would they change?

Status: literature-backed analysis and a conditional extension of the
[native constructive construction](2026-09-25-native-constructive-sjas.md).
The conditional consistency argument below is supplied here. It is not an
attribution of an SJAS theorem to the cited deep-inference or graph papers.
No new executable checker, arithmetized graph checker, or mechanization is
claimed. Targeted searches and the previously recorded Willard corpus audit
did not establish an existing native cirquent/deep-inference/proof-net SJAS.
That is a statement about the evidence reviewed, not a proof that none exists.

The answer is affirmative in two senses. These methods can serve as proof
search, proof representation, or execution mechanisms while the original
checker remains authoritative. They can also be candidates for a new native
checker, with its own self-consistency assertion and consistency proof.

## What the alternatives provide

| Method | Role | Main opportunity | Additional specification needed |
| --- | --- | --- | --- |
| Cirquent calculus | Deduction over circuit-shaped logical structures with shared parts | Compact formulas and proofs; explicit resource sharing | The intended resource semantics, arithmetic quantifiers/equality, and a constructive interpretation if required |
| Calculus of structures / deep inference | Rules apply inside logical structures, subject to polarity and binding conditions | Local structural steps and sometimes much shorter proofs | Exact rule set, treatment of nonlogical axioms, binding, and certificate encoding |
| Proof nets | Graph representation of proofs modulo some irrelevant sequential choices | Direct account of dependency and cut reduction | Correctness criterion, typing, quantifiers, and permitted structural/exponential links |
| Interaction nets | Local graph rewriting and distributed execution | Explicit duplication/erasure and parallel reduction | A logical typing/correctness discipline or a verified compiler from accepted proofs |

These are not four mutually exclusive choices: cirquent calculi can use
deep inference, and proof nets can be compiled into interaction systems.
Neither a net's appearance nor its ability to execute makes it an arithmetic
deduction system by itself.

Japaridze's CL8 has polynomial-size proofs of the propositional pigeonhole
principle (Theorem 8.6). Its cirquents also have a resource interpretation;
the classical fragment does not exhaust that interpretation. Consequently
cirquent sharing must not automatically be identified with ordinary formula
DAG compression. The cited CL8 presentation does not by itself supply the
first-order intuitionistic arithmetic apparatus required here.

Bruscoli and Guglielmi prove polynomial equivalence between their classical
SKS/SKSg systems and Frege systems. Their analytic KS systems have polynomial
proofs of Statman tautologies, giving an exponential advantage over the
analytic Gentzen systems compared in the paper. This does not establish that
every analytic deep-inference system polynomially simulates Frege. Nor does
shorter proof existence guarantee easier proof search.

For a constructive candidate, Tiu's first-order intuitionistic systems are
particularly relevant: Theorems 4-5 relate SISgq/ISgq to intuitionistic
sequent calculus, and Theorems 10-11 give the atomic-contraction variants
SISaq/ISaq. These are logical equivalence results, not SJAS results. Equality
and the chosen arithmetic axioms must still be incorporated explicitly.

Gundersen, Heijltjes, and Parigot's atomic lambda-calculus provides a separate
computational precedent: intuitionistic deep inference yields typed terms
with explicit sharing, and strong normalization is established for their
typed calculus. That theorem does not cover the proposed arithmetic and
self-consistency extension without an additional interpretation argument.

## Preserve the existing checker as an initial route

Keep the original theory I and its exact Hilbert checker. A new proof engine
may search using deep inference or nets and emit a proof accepted by that
checker. A compiler can also translate accepted constructive proofs or their
extracted realizers to an interaction-net evaluator.

For a proof-producing frontend, the trusted acceptance condition remains
the original proof check. If both a graph and an original certificate are
required, the paired acceptance argument in the
[PTS note](2026-09-26-pts-presentation-of-constructive-sjas.md#clarification-exactly-which-consistency-statement-is-available)
applies. This establishes the stated consistency guarantee for paired
acceptance, not for an independent graph checker.

This route can improve search and execution immediately at the design level,
but expanding certificates may forfeit graph compression at the final
checking boundary. Actual performance improvements require implementation
and measurement.

## A conditional native construction

The finite-model proof in the native constructive note needs semantic
soundness and a certificate-size bound. It does not use a special induction
on the shape of Hilbert derivations. The following extension isolates its
requirements.

Retain exactly the arithmetic base B of that note: non-growth functions,
names C_i with standard value 2^(i-1) for i >= 1, initial axioms for 0,1,2,
the separate naming axioms a_i = Add(C_i,C_i,C_(i+1)), and the selected true
2-reduced universal bounded axioms. In particular, do not add uniform
successor/addition/multiplication totality or induction.

Let D satisfy these conditions:

1. Accepted certificates establish arithmetic consequences of a finite,
   explicitly recorded set of nonlogical axioms. D is sound from that set
   in the standard structure and in the finite Boolean structures used
   below. A graph calculus may discharge this by a sound interpretation of
   its internal structures. Recognition of an axiom as a proof is available.
2. There is a faithful bounded arithmetic predicate for D-certificates from
   B plus an optional extra axiom, and the bounded substitution interface
   needed to construct the self-referential sentence. Merely saying that a
   checker is computable or polynomial-time does not supply this result.
   Supplied verification traces are permitted, provided their verification
   is bounded in the chosen language and included in the certificate code.
3. Certificates account for the actual nonlogical axioms they use. A proof
   using the extra axiom includes its expanded arithmetic formula, including
   numeral/name information hidden by any displayed abbreviation. Sharing
   is allowed, but cannot hide the finite axiom support or that information.
4. For a certificate code p, let L be the number of distinct naming axioms
   in its support, and let r be max(2, the greatest C_i index in the expanded
   self-consistency sentence). If the certificate uses that sentence, its
   coding guarantees floor(log2 p) > r + L. For example, store unary name
   annotations and an explicit axiom manifest in an N-bit record, then use
   the previous padding convention floor(log2 p) = 128 + 4N.

Use the bounded fixed-point interface to form

```text
S_D = B + H_D, with deduction method D;

H_D: forall a,p.
       not (FalseCode(a) and Prf_D_S_D(a,p)).
```

The predicate covers proofs using H_D itself. H_D is an axiom, so S_D
proves H_D. Consistency follows under conditions 1-4:

1. Suppose p is the least numerical code of a D-proof of FALSE. Since B is
   true in the standard structure, p must use H_D.
2. Choose the least k >= r such that naming axiom a_(k+1) is absent from
   the support of p. There are at most L distinct naming axioms in that
   support, so k <= r + L.
3. Take M with domain 0,...,2^k. Give grounding functions their usual
   non-growth interpretations. Interpret C_i normally for i <= k+1 and
   interpret higher names as 0. All naming axioms except a_(k+1) hold.
   All other B-axioms in the proof hold by the same bounded absoluteness
   argument as before.
4. D's soundness in M forces H_D to be false there. Every constant occurring
   in H_D has its standard value because k >= r. Its bounded matrix is
   absolute between M and the standard natural numbers, so its counterexample
   supplies a genuine D-proof of FALSE with code q <= 2^k.
5. But q <= 2^k <= 2^(r+L) < p, contradicting the choice of p.

Therefore S_D is consistent. Its self-consistency axiom is standardly true;
all its axioms are true, so the same semantic soundness assumption gives
external standard-model soundness of its arithmetic conclusions.

This proves the conditional extension, rather than leaving its central
consistency implication as a conjecture. Instantiating it for a particular
native graph checker still requires conditions 1-4, especially bounded
arithmetization and the exact fixed-point construction. The conclusion is
ordinary no-FALSE-proof consistency, not Willard's technical Level(1).

## Why sharing can survive this construction

The bound uses the number of distinct naming axioms in the support, not
the number of times a shared proof is reused. A proof DAG can have many
references to one stored subproof without increasing that support. Its
unfolding need not be charged in full for the argument above.

Thus the original note's exclusion of formula sharing is a choice of that
checker, not a universal SJAS prohibition. A new checker must recover the
required bounds, but it need not prohibit all pointers or memoization.
There is a direct historical caution against such a prohibition: Willard
1993, printed p. 327, explicitly permits storing a long numerical constant
once and referring to it by pointers, and says this slightly strengthens
his results. This does not authorize arbitrary compressed proof macros.

Keep three claims distinct: a graph reuses stored data; a logical calculus
admits contraction; and a weak arithmetic theory certifies uniform
construction/duplication/composition of encoded proofs. None is an automatic
substitute for either of the others.

## Effect on self-consistency and the G2 boundary

For a fixed axiom set, equivalent deductive presentations preserve the
ordinary theorem set externally. They need not have internally interchangeable
consistency assertions. Regenerating the self-consistency axiom also changes
the axiom set, so equivalence of the underlying pure logics does not by itself
establish equivalence of the resulting self-justifying theories.

For example, suppose a theory T proves both

```text
forall h. HilbPrf_T(FALSE,h) -> exists d. DPrf_T(FALSE,d)
forall d. not DPrf_T(FALSE,d).
```

Then intuitionistic existential elimination immediately yields

```text
forall h. not HilbPrf_T(FALSE,h).
```

For arithmetic and coding to which Hilbert G2 applies, that can rule out
the proposed D self-consistency theorem. In some stronger Willard profiles,
restrictions on deduction and proof growth are therefore essential. An
external polynomial simulation alone does not establish the displayed
internal statement: code existence and arithmetic correctness of the
transformation are additional requirements.

Our present weak construction already uses Hilbert modus ponens and full
logical contraction and proves its selected Hilbert consistency statement.
The stronger-profile obstruction must not be transferred to it without
checking the arithmetic hypotheses. Deep inference or sharing is not itself
what crosses the boundary, and an analytic/cut-free presentation alone is
not a sufficient reason to expect self-justification.

## Constructive computation and graph correctness

To retain the previously requested witness extraction, choose D with an
effective translation into ordinary intuitionistic derivations over B+H_D,
or give D an independent realizability interpretation. The bounded axiom
realizers and the negative H_D realizer are then assigned as before, using
the external consistency proof for their validity. Classical CL8 or SKS
does not automatically preserve that interpretation.

For proof nets, a schematic checker is

```text
NetPrf(A,n,w) :=
  FormulaAndBindingCheck(A,n)
  and CorrectnessCertificate(n,w)
  and NonlogicalAxiomCheck(n).
```

The correctness condition depends on the chosen logic. Local labels alone
are insufficient in familiar proof-net systems. Efficient checking is
possible in specified fragments: Nguyen proves linear-time correctness and
quasi-linear sequentialization for MLL+Mix nets. Those bounds are not a
theorem about arbitrary first-order arithmetic nets with exponentials.

For interaction nets, distinguish an accepted typed proof graph from its
execution. Local rewrites can implement cut reduction or the evaluation of
extracted programs. Lafont's general interaction combinators are a universal
computational model; a logical interpretation must select the appropriate
typed/correct nets. That universality does not yield extra arithmetic
totality theorems in an SJAS. A supplied finite execution trace can be
checked without asserting termination of every execution.

If normalization is used to argue consistency, it must be accompanied by
subject reduction and the absence of closed normal proofs of FALSE, with
all arithmetic and self-consistency constants accounted for. A normalizing
calculus with an arbitrary constant of type FALSE is still inconsistent.
Proof/program correspondence does not require each runtime graph rewrite
to be a separately certified arithmetic deduction.

For this project's constructive aim, a first-order intuitionistic
deep-inference calculus is a concrete candidate for native deduction;
explicit-sharing terms or typed nets are candidates for proof representation
and execution. Combining those components into one native SJAS remains a
checker-specific construction, with the conditional argument above supplying
the consistency route.

## Sources and verification

- [Japaridze, Cirquent calculus deepened](https://arxiv.org/abs/0709.1308),
  CL8, Theorems 5.4 and 8.6, and its distinction between classical and
  resource interpretations.
- [Bruscoli and Guglielmi, On the Proof Complexity of Deep Inference](https://arxiv.org/abs/0709.1201),
  Theorem 3.12 and Section 4. Polynomial equivalence here concerns the
  specified classical systems, not every analytic variant.
- [Tiu, A Local System for Intuitionistic Logic](https://users.cecs.anu.edu.au/~tiu/papers/localint.pdf),
  extended author version, Theorems 4-5 and 10-11.
- [Gundersen, Heijltjes, and Parigot, A Proof of Strong Normalisation of the Typed Atomic Lambda-Calculus](https://purehost.bath.ac.uk/ws/files/18737134/AL_LPAR.pdf),
  2013, Sections 1-4. Web-extracted primary text checked; local PDF download
  returned HTTP 403 and web rendering failed, so no rendered-page check is
  claimed for this source.
- [Nguyen, Unique perfect matchings, forbidden transitions and proof nets for linear logic with Mix](https://arxiv.org/abs/1901.10247),
  LMCS 2020, introduction and the MLL+Mix correctness/sequentialization scope.
- [Lafont, Interaction Combinators](https://chorasimilarity.wordpress.com/wp-content/uploads/2024/01/ic-lafont-1.pdf),
  Information and Computation 137 (1997), 69-101; mirrored original paper,
  especially printed p. 70 on distributed computation and universality.
- [Willard 1993, local original](../../lit/danwillard1993.pdf), printed
  p. 327, on deduction choices, pointer sharing, and supplied checker traces.
- [Willard 2006, local author version](../../nachlass/papers/willard2006_generalization_second_incompleteness_author_apal6.pdf),
  Theorem 3; the native constructive note records the imported bounded
  arithmetization and its proof-status qualifications.

Rendered pages inspected during this investigation: Japaridze PDF p. 39;
Bruscoli-Guglielmi PDF p. 15; Tiu PDF pp. 8 and 12; Nguyen PDF p. 1;
Lafont PDF p. 2 / printed p. 70; Willard 1993 PDF p. 3 / printed p. 327.
Source downloads and renderings were temporary research artifacts under
`/tmp/sjas-alternative-deduction-20260926/`. Documentation validation does
not amount to a machine-checked metatheorem.
