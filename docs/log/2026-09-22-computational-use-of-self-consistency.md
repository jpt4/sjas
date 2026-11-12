# Computational use of an SJAS self-consistency proof

Date: 2026-09-22 (America/Indiana/Indianapolis).

## User correction

> which proof-term interpretation turns the available specifications into accepted programs, while retaining the intended proof apparatus in the self-consistency statement <- This is the whole of the question - what are the effects of being able to write programs with access to the self-consistency statement?

The preceding numerical-function analysis did not answer this central question.
The analysis here concerns the additional proof/program constructions obtained
by exposing self-consistency itself. It does not use the additive output bound.

## The available interface

Fix the proof predicate of the chosen system S, including its self-consistency
axiom. Write

    CheckedProof_S(A) = Sigma (p : Code). check_S(p, code(A)) = true
    con_S : CheckedProof_S(False) -> False

Here False is the empty proposition/type, and the checker is exactly the one
named by this consistency assertion. The notation displays the computational
reading of the logical formulas; it does not replace that checker by an
unspecified host type checker.

For the formulation that excludes opposed proofs in a specified formula class,
the corresponding interface is instead

    con_S^1 : CheckedProof_S(A) -> CheckedProof_S(not A) -> False

with A restricted to that class. One must use the available consistency
formulation, rather than silently assume an internally certified transformation
of any two opposed proofs into a proof of False.

## An exact new elimination principle

With ordinary sum types and empty elimination:

    eliminate : A + CheckedProof_S(False) -> A
    eliminate (left a)  = a
    eliminate (right p) = absurd (con_S p)

Self-consistency therefore allows an interface with this particular error case
to be exposed as an interface returning A. For a more general error type E,
the same construction works when a certified map E -> CheckedProof_S(False)
has been supplied. The producer must already be total if the conclusion is a
total interface; exclusion of errors alone is not a termination proof.

This family of elimination principles has exactly the logical strength of
con_S: it follows from con_S, and its instance A=False recovers con_S by applying
eliminate to a right injection. This identifies an actual change in available
typing derivations, not just an informal analogy.

## A certified optimization of the system's own verifier

Suppose the checker is exposed as its total Boolean decision operation. Then
con_S yields

    forall p, check_S(p, code(False)) = false.

For each p, case-analyze the Boolean result. A false result supplies the desired
equality. A true result, together with its equality evidence, constructs an
element of CheckedProof_S(False), to which con_S applies. Empty elimination
discharges that branch.

Consequently the program which checks whether a candidate proves False can be
replaced by the constant function returning false, with an internal proof that
the returned Boolean is unchanged. This is a certification of output
equivalence, not a claim that their costs or execution traces are equal.

The same axiom establishes that a procedure which halts only after finding an
accepted contradiction proof has no successful finite run, once its ordinary
run-to-accepted-certificate invariant has been proved. It does not construct a
uniform code of that procedure's infinite execution.

## Using the axiom for another program's safety

Let Bad(e,t) mean that t witnesses a forbidden execution of e. Suppose an
internally certified construction is available with type

    collapse_e : (t : Trace) -> Bad(e,t) -> CheckedProof_S(False).

Then the safety proof/program is the explicit composition

    safe_e : (t : Trace) -> Bad(e,t) -> False
    safe_e t bad = con_S (collapse_e t bad).

The point is that a conditional invariant (failure would produce an accepted
contradiction) becomes an unconditional exclusion of failure. The reduction is
a supplied proof/program, not a consequence of self-consistency. This formula
does not assume arbitrary proof reflection or proof composition.

A successor consistency certificate has the same shape. Given

    lower : CheckedProof_Sprime(False) -> CheckedProof_S(False),

one obtains

    con_Sprime q = con_S (lower q).

This is a sufficient, explicit acceptance condition for the successor's
consistency. It neither constructs lower automatically nor proves arbitrary
semantic safety of the successor.

## The concrete Willard reflection result

Willard's Theorem 6.12 supplies a further application for the stated EA-stable
Level(1) construction S=B+SelfCons^1(B). For each relevant Pi_1 sentence A it
proves the translated reflection principle

    CheckedProof_S(A) -> A^T.

Example 6.7 and the proof of Theorem 6.12 take A^T to express that there is no
base-system B proof of not A. A proof of A in S and a B proof of not A would
give the opposed S proofs excluded by the Level(1) assertion. In schematic
proof-term notation the operation applies con_S^1 to the first certificate and
the included base-system refutation.

The source proves that A^T and A agree in the standard model, using the base
system's soundness and Sigma_1 completeness. Their interchangeability inside S
is not provided; Remark 6.14's footnote explicitly distinguishes this. Thus a
program receives an internal no-refutation guarantee from a supplied proof
certificate. Turning it into the original A requires a further bridge for that
particular A. Treating the translation as an unrestricted unbox operation would
erase the essential restriction.

Source: Dan E. Willard, *A Detailed Examination of Methods for Unifying,
Simplifying and Extending Several Results About Self-Justifying Logics* (2011),
[Example 6.7](https://arxiv.org/pdf/1108.6330#page=24),
[Theorem 6.12](https://arxiv.org/pdf/1108.6330#page=27), and
[Remark 6.14, footnote 15](https://arxiv.org/pdf/1108.6330#page=28).
These pages were rendered and visually checked during this response.

## Computational significance and status

The new capability is internally justified elimination of particular error
cases and construction of particular correctness, nontermination, and
consistency certificates. The axiom is a negative proof constant; by itself it
specifies no new numeral-producing reduction rule, general recursor, or
arbitrary-proof evaluator. A computational interpretation may erase its proof
uses while retaining a more strongly specified executable program. It is not
claimed that all interpretations erase it, that it never affects certified
termination, or that adding it preserves a fixed class of provably total
implementations.

These are explicit logical derivations and schematic proof terms. The Willard
reflection theorem is an imported paper theorem. No new language
implementation or mechanization is claimed.
