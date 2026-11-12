# Internal versus external consistency for programming languages

Date: 2026-09-22

The user requested a first-principles and literature-based comparison of a
language able to use its own consistency statement with a consistent language
whose consistency proof exists only in an external metatheory. They explicitly
rejected merely repeating existing repository proposals. Mathematical notation
here is plain text suitable for a terminal.

This note records new comparative arguments. The derivations below are paper
proofs under explicit hypotheses, not mechanized results or claims that an SJAS
compiler implements the required interfaces.

## What is being compared

Let T name a specified proof system, including its axioms, deduction apparatus,
proof representation, and checker. Write:

```text
Con(T) = for every code p, p is not an accepted T-proof of False.
```

An external consistency theorem is a proof of this statement in a metatheory M.
An internal consistency theorem is an accepted T-proof of this statement about
T itself. Actual consistency is a separate property: a contradictory theory can
also prove a sentence saying it is consistent.

Given an external proof of Con(T), internal availability supplies no additional
absence of contradictory proofs. It changes where the fact can be used and
which checker can accept the resulting certificates. This matters for a policy
requiring a T-proof of each admitted program's contract. It does not establish
an advantage over an architecture that already admits suitable M-certificates.

For genuine self-consistency, the quantified proofs must include proofs using
the consistency theorem. If S = T + Con(T), then S has a consistency theorem
about T, not automatically about S. Replacing the old checker by the new one in
the consistency formula is a substantive change.

## An exact difference in certification

Work with PA, or a base theory able to formalize the following ordinary machine
semantics, prove the chosen proof checker total, and prove the behavior of the
explicit divergent branch. Let T be an effectively presented target theory with
a suitable primitive recursive proof predicate.

Consider this raw program:

```text
fragile(p):
    if check_T(p, False):
        loop forever
    return 0
```

The base proves:

```text
fragile terminates for every p  <->  Con(T).
```

Left-to-right: if q were an accepted contradiction proof, the machine on input
q would take the divergent branch, contradicting its asserted totality.

Right-to-left: Con(T) makes the check false for every input; the total checker
therefore finishes and the machine returns zero.

Consequently, a native consistency premise certifies this particular
implementation as total. Where Con(T) is not natively provable, neither is its
totality, under the stated equivalence. An external proof of Con(T) can already
certify the same implementation externally.

If T is actually consistent, fragile computes the constant-zero function.
The implementation that simply returns zero already has an elementary
termination proof. The new certificate therefore does not imply a new numeric
function. This example concerns an architecture admitting raw programs through
totality certificates; it is not a claim that arbitrary recursion becomes a
term of every proof calculus.

For the specific comparison PA versus PA + Con(PA), the second incompleteness
theorem supplies nonprovability of the first theory's consistency, assuming PA
consistent and using its standard proof predicate. The extension certifies the
displayed PA-checking program, but is not thereby a self-consistent theory.
Applying the same example to a genuine weak SJAS requires re-establishing the
checker and machine-semantics assumptions in that actual apparatus.

## Why this need not add provably computable functions

Kreisel's observation is that adding true Pi_1 sentences to appropriate theories
does not change their provably recursive functions. Ferreira discusses it in
Section V, PDF page 7, of [his 2008 paper][ferreira]. For PA, a provably recursive
function means a numerical function with some program whose totality PA proves.
This definition does not fix the implementation or demand that PA prove every
externally true specification of that function.

Here is a direct classical arithmetic derivation of the relevant case. Let C be
a true sentence of the form "for every n, R(n)", with R primitive recursive.
Suppose PA + C proves that a program e terminates on every input. By the
deduction theorem, PA proves C implies the totality of e.

Construct a program e_star:

```text
e_star(x):
    interleave these two computations fairly:
        run e(x); if it halts, return its result
        search n for not R(n); if found, return 0
```

PA proves e_star total by classical cases. If C holds, the first computation
halts by the conditional totality theorem. If C fails, a finite counterexample
exists and the second computation halts. Standard formalized simulation and
finite-search reasoning justify this argument in PA.

Externally, C is true, so the second computation never succeeds. Therefore
e_star computes the same function as e. Conversely every PA-provably recursive
function remains so after adjoining C. This proves equality of the extensional
classes. A proof using arbitrarily many available true Pi_1 axioms actually uses
only finitely many; their conjunction has the same relevant form.

There is no claim that PA proves e and e_star equivalent, that PA proves e
itself total, or that PA proves the original extended-theory specification of
the function. In particular, this is not Pi_1 or Pi_2 conservativity. It is a
statement about the class of functions admitting some provably total
implementation. It supplies no proof-size or runtime-cost equality either.

For a consistent effectively presented T with the chosen usual proof predicate,
Con(T) is such a true Pi_1 statement. Thus this benchmark directly refutes an
unconditional inference from access to a consistency statement to a larger
class of provably computable numerical functions. The proof uses PA's resources;
it has not been transferred to Willard's weaker calculi.

## What the self-reference adds

The distinctive internal capability concerns the current proof apparatus:
certification arguments may use consistency while referring to a collection of
proofs that includes those very arguments. A theorem only about a predecessor
does not close this particular reference back to the current checker.

This capability is confined to its stated proof predicate. It does not certify
arbitrary additions of axioms or proof rules, a replacement checker, all
successor programs, or the semantics of an implementation. A strong external
metatheory can reason about that same fixed checker, including its self-using
proofs, so closure inside the native proof system is the comparative benefit.

In ordinary sufficiently strong effective systems with standard provability,
G2 obstructs native consistency. Genuine examples therefore require identifying
the hypotheses that fail; one cannot assume an unchanged conventional strong
proof system and append its own consistency without cost.

Willard's [2011 paper][willard], Theorem 6.12, establishes an actual restricted
internal use for EA-stable Level(1) systems: a checked proof of a permitted
universal statement yields its specified translated statement internally. The
translation preserves truth in the standard model; the source system does not
generally prove the original and translated statements equivalent. This is not
unrestricted soundness reflection. Remark 6.16 distinguishes the internal
self-consistency axiom from the external justification for its compatibility.

Reading a permitted translated statement as a program contract still requires
a faithful proof/program interpretation. Its untransformed operational meaning
cannot be substituted using an equivalence available only externally.

## Comparison with established programming practice

Appel's [foundational proof-carrying code][appel] places program acceptance at a
small checker and supplies derivations connecting code with a safety policy.
The practical question is which certificates that checker accepts and which
semantic theorems justify its policy. Internal self-consistency does not supply
those semantic connections merely by asserting absence of contradiction.

The [2014 CakeML paper][cakeml] demonstrates a verified compiler that compiles
itself to verified machine code, with the development carried out in HOL4.
Thus externally verified self-compilation is already possible without an
object-language self-consistency theorem. CakeML permits divergence; this is an
example about verified implementation architecture, not an identification of ML
with a consistent total proof calculus.

An external theorem can also justify eliminating the forbidden branch of
fragile or certifying an admission procedure. An internal theorem matters when
the resulting certificate must itself belong to the native proof system. Its
existence alone proves no reduction in proof-search cost, certificate size,
execution time, or trusted implementation size.

The resulting established benefit is internal availability and composition of
particular certification arguments, covering the current specified apparatus.
The further questions about a particular language concern exactly which
contracts become natively provable and whether their certificates have useful
computational interpretations. The property "self-consistent" by itself does
not settle those questions or replace an external consistency argument.

## Source and validation record

The source review used the papers themselves, not repository assertions:

- Ferreira, *A most artistic package of a jumble of ideas* (2008), Section V,
  PDF page 7: Kreisel's observation and functional-interpretation context.
- Willard, *A Detailed Examination of Methods for Unifying, Simplifying and
  Extending Several Results About Self-Justifying Logics* (2011): definition
  on PDF page 2, Theorem 6.12 on page 27, and Remark 6.16 on page 29. The
  standard-model/internal-equivalence distinction also appears in footnote 15
  on page 28.
- Appel, *Foundational Proof-Carrying Code* (2001), PDF page 1: the safety
  certificate/checker architecture and its trusted foundations.
- Kumar, Myreen, Norrish, and Owens, *CakeML: A Verified Implementation of ML*
  (2014), PDF page 1: verified compilation, bootstrapping, and HOL4.

These load-bearing pages were rendered and visually inspected. Aaronson's
2003 survey was a cross-check for the explicit PA formulation of Kreisel's
lemma; it is not used to establish unrelated claims in that discussion.
Kreisel's original 1951 Part I bibliographic record was located, but its full
text was not reviewed. The direct interleaving proof above avoids treating an
unread historical attribution as a substitute for an argument.

No runtime experiment or code test can establish these universal metatheoretic
claims. No implementation or mechanized theorem is added by this note.

[ferreira]: https://webpages.ciencias.ulisboa.pt/~fjferreira/dialectica_ff_web.pdf
[willard]: https://arxiv.org/pdf/1108.6330
[appel]: https://www.cs.princeton.edu/~appel/papers/fpcc.pdf
[cakeml]: https://cakeml.org/popl14.pdf
