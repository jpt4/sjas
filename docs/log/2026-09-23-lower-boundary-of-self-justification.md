# The lower boundary of self-justification

Date: 2026-09-23

The user asked how far self-justification extends downwards, especially whether
there are sub-Turing self-justifying systems below the family studied by Willard.
This continues the first-principles and literature review of what internal
consistency supplies to a programming language.

Status: source-checked mathematical analysis. The halting reduction below is a
paper derivation. No new decidable self-justifying calculus or mechanized theorem
is established here. A least natural system has not been identified by this
review; this is not a theorem that none exists.

## Three different strength questions

The following properties must be specified separately:

1. Whether execution of accepted programs can realize arbitrary partial
   recursive functions.
2. Whether the logic can express arbitrary finite computation histories and
   prove the existential assertion for each actually halting computation.
3. Whether theoremhood has a terminating decision procedure.

A total language can lack the first property while its verification logic has
the second and lacks the third. Primitive recursive proof checking does not
imply decidable theoremhood: the unbounded search over certificates is a
different operation.

Consequently Willard's arithmetic constructions should not, without a specified
program interpretation, be assigned a single class of executable programs.
Their familiar restrictions on total arithmetic functions concern internal
uniform totality theorems. They do not remove the ability to represent
individual arbitrary finite computations.

## A lower threshold built into Willard's general framework

In [Willard 2011][willard], Definition 3.4 requires:

- A numeral term for every standard natural number.
- Bounded formulas representing the graphs of addition and multiplication.
- A base with true standard-model axioms that proves every true sentence in
  its existential, bounded-matrix class, Sigma_1^xi.

Lemma 3.6 supplies an effective translation of ordinary bounded arithmetic into
the chosen bounded language, preserving standard-model meaning. Prefixing
existential quantifiers therefore allows ordinary halting assertions to be
translated into Sigma_1^xi sentences.

Here is an explicit consequence of those requirements. Fix a sound, effectively
axiomatized instance S retaining this base and the usual sound deduction method.
For each machine e, effectively form:

```text
H_e = there exists an accepting finite computation of e on blank input
```

Use Lemma 3.6 to express this in the native existential class. Then:

```text
e halts  <->  S proves H_e.
```

Proof: if e halts, its finite computation makes H_e a true existential sentence,
which the base proves by its completeness requirement. If S proves H_e,
standard-model soundness makes H_e true, so e halts. The translation is uniform
and effective. Thus halting many-one reduces to theoremhood. Effective
axiomatizability gives the converse reduction of theoremhood to halting through
proof enumeration. The theorem set is computably enumerable complete.

This is a statement about theoremhood, not a theorem that the language accepts
a general recursive evaluator as a total program. Soundness is an explicit
hypothesis; consistency alone was not substituted for it.

Therefore a decidable-theoremhood example cannot be obtained merely by choosing
a weaker sound effective member while retaining all these generic-configuration
requirements. One must change the framework's lower representational or
completeness requirements. This conclusion concerns the specified 2011 generic
framework, not an assertion about every conceivable self-justifying logic.

## An independent weak positive example

[Pakhomov 2019][pakhomov] constructs a first-order weak set theory H_<omega
proving a natural arithmetization of its own Hilbert-style consistency.

The base H has extensionality, separation, and a unary hierarchy operation with
a defining axiom. H_<omega adds the existence of each individual finite von
Neumann ordinal. Every finite subtheory has a finite model, while the full
theory proves the existence of all individual hereditarily finite sets. It does
not prove uniform successor totality for its internal natural numbers.

Theorem 5.4 proves, in the paper's notation:

```text
H proves NAT(Con_pred(H_<omega)).
```

Since H is a subtheory of H_<omega, the latter proves its own specified
consistency sentence. This is an actual self-consistency result, rather than
just failure of a proposed incompleteness argument.

Appendix A establishes that H_<omega and Robinson arithmetic R are mutually
interpretable. This gives a precise weak interpretability benchmark. It does
not give a decidable example: R is essentially undecidable, so a consistent
theory interpreting R cannot have decidable theoremhood. Otherwise its
decidable consequences under the interpretation would give a consistent
decidable extension of R.

This result is not asserted to be strictly weaker than every Willard system.
Nor does mutual interpretability alone transfer the result to R's own native
consistency formula. The translation of proof representations and consistency
statements still matters. Pakhomov explicitly discusses that issue at the end
of Appendix A.

## Why standard weak candidates do not immediately settle the question

A primitive recursive executable language is sub-Turing in the sense of not
representing arbitrary partial recursive execution. Nonetheless ordinary PRA
with its usual arithmetic proof apparatus falls under the second
incompleteness theorem. Restricting executable recursion is therefore not by
itself a construction of self-justification.

Presburger arithmetic has decidable theoremhood, but its ordinary language
does not provide the needed internal account of its complete syntactic proof
apparatus. Its external consistency proof should not be reclassified as an
internal theorem simply because its decision procedure terminates.
[Franks][franks], introduction, treats precisely this distinction, including
the related historical example of Herbrand's weak arithmetic.

Likewise, checking a bounded collection of proofs gives bounded consistency of
an unbounded calculus. To make that global consistency of a genuinely bounded
calculus, the bound must be part of the definition of the actual calculus and
the internal statement must cover all its accepted certificates, including the
certificate being presented. A finite model checker used externally does not
automatically establish internal self-justification.

Ordinary modal typed-code elimination, such as Box(False) -> False, also does
not by itself prove the universal statement about a raw syntactic checker.
Identifying a modal code type with all certificates accepted by that checker
requires an additional adequacy argument. This review does not count such an
unproved identification as a positive example.

## Consequences for a sub-Turing target

A language of restricted executable programs can use a stronger, self-justifying
logic for certification. That is a possible separation of roles, but it does
not lower the theoremhood complexity of the certifying system.

For an explicit simple construction, choose a sound Type-A SJAS S with its
specified checker, and use only first-order numeric expressions as executable
programs: input variables, numeric constants, addition, maximum, and conditionals
with terminating primitive tests. Attach S-certificates to contracts, with the
proof judgments and consistency predicate remaining exactly those of S.
Structural induction on expression syntax proves that evaluation terminates;
there is no unbounded recursion or search. The internal S-consistency theorem
is available to the contract proofs. This is a sub-Turing executable language
using a self-justifying logic, not a decidable SJAS or a lower-strength proof
theory. It is also not a claimed Curry-Howard translation of every S-derivation.

The stronger target is a decidable proof calculus S together with:

```text
an explicit finite-certificate checker Check_S;
an internal statement quantifying over every candidate S-certificate;
a native derivation that no such certificate proves False;
an external consistency proof for the full S, including that derivation.
```

The internal statement must retain the selected checker and proof apparatus.
No theorem reviewed here makes Turing completeness a necessary condition for
this package. Conversely, no natural decidable instance of the full package was
established by the sources reviewed here. The verified results place a positive
example at R's interpretability level and show that Willard's generic class
cannot itself supply a sound effective decidable-theoremhood instance.

"Weakest" also needs an ordering: inclusion of axioms, interpretability,
runtime functions, and theoremhood complexity are different comparisons.
Deleting axioms from a self-justifying theory need not preserve the proof of
consistency, and changes the proof predicate that the new statement must name.

## Sources and verification

- Willard, *A Detailed Examination of Methods for Unifying, Simplifying and
  Extending Several Results About Self-Justifying Logics* (2011), Definition
  3.4 and Lemma 3.6, printed pages 9-10 (PDF pages 10-11).
- Pakhomov, *A weak set theory that proves its own consistency* (2019),
  construction on page 3, Theorem 5.4 on page 23, Appendix A on page 24.
- Curtis Franks, *The metamathematics of very weak arithmetics*, available
  manuscript dated October 30, 2005, introduction on page 2.

These mathematical pages were rendered and visually inspected. Search results
for finite-model truth definitions, staged computation, and self-verifying
automata were screened; no stronger raw-checker self-consistency result is
claimed on their basis. Failure to locate a decidable example is a limitation
of this review, not a proof of impossibility or a claim that the literature has
settled the question negatively.

[willard]: https://arxiv.org/pdf/1108.6330
[pakhomov]: https://arxiv.org/pdf/1907.00877
[franks]: https://www.lps.uci.edu/files/grad-alumni/CFranks/veryweak.pdf
