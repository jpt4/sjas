# Two computational boundaries for self-justification

Date: 2026-09-23

The user clarified the lower-boundary question:

> Both; distinguish the two boundaries

This refers to executable computation and the logical representation of
arbitrary finite computations. It continues the
[lower-boundary analysis](2026-09-23-lower-boundary-of-self-justification.md).
The later instruction "continue" resumed this interrupted comparison.

Status: source-checked research with explicit paper derivations. There is no
new implemented or mechanized calculus. The arguments below establish a
separation and several precise bounds; they do not identify a least
self-justifying system or establish a decidable example of the full package.
Notation is written for CLI reading without raw LaTeX.

## Specify execution, representation, and certification separately

Fix a program language L, a logic S with a specified deduction apparatus, and
a certificate checker. The following are different properties:

| Property | Operational or mathematical test |
| --- | --- |
| Universal execution | L realizes every partial computable numerical function. |
| Universal finite-computation representation | There is an effective translation from arbitrary machine/input pairs to sentences true exactly when the machine halts. |
| Adequate witness proofs | S proves the translated halting sentence whenever a genuine finite halting computation exists. |
| Complete totality certification | Every total computable numerical function has an implementation with an accepted totality certificate. |
| Decidable theoremhood | An algorithm decides whether an arbitrary S-sentence has an S-proof. |

The second property concerns standard-model meaning. The third adds a
provability requirement. Soundness for the translated halting sentences is
another requirement, and is stated explicitly when used below. Decidability
of checking one proposed proof supplies neither decidable theoremhood nor
complete totality certification.

For this discussion, self-justification requires an internally proved
consistency sentence for the selected S-checker and an external consistency
argument for S. The quantified certificate domain must cover every actual
candidate certificate. The checker must include the self-consistency proof
itself when determining which proofs are accepted.

## Execution can be restricted while self-consistency remains available

[Meyer and Ritchie][loop], pages 465-466, give a useful execution benchmark:
LOOP programs use iteration counts fixed on entry to each loop, always
terminate, and compute precisely the primitive recursive functions. They
therefore do not realize arbitrary partial computable execution.

Here is a direct compatibility construction. Choose a sound self-justifying
logic S with its original certificate checker. Restrict executable expressions
to constants, variables, addition, maximum, and conditionals with terminating
primitive tests. An expression contains only finitely many subexpressions and
no recursion. Structural induction on its syntax proves termination for every
input. Program contracts can carry S-proofs, and those proofs can use S's
existing self-consistency theorem.

Restricting these executable expressions does not change S, its proof
predicate, or its self-consistency theorem. This proves that even a loop-free
executable component can coexist with self-justifying certification. The
certifying logic retains its original representational strength.

A minimal example connects the components without assuming that S internally
proves a general evaluator correct. Let the program return Boolean false for
every candidate proof code p. Its contract includes

```text
not Prf_S(code(False), p).
```

The universally quantified version of this contract is exactly the selected
self-consistency theorem. The executable program terminates immediately. To
identify its output with an actual checker's Boolean output, one must also
establish that the implementation implements Prf_S; that adequacy claim is
separate from self-consistency.

This construction does not establish a Curry-Howard interpretation of all
S-proofs or a self-verifying implementation whose entire proof calculus has
the restricted expressive strength. Neither does it establish that S certifies
every primitive recursive program. Those claims require additional theorems
about the specific program and proof representations.

Restricting executable recursion alone is also insufficient to obtain
self-justification. In the ordinary first-order presentation of PRA, the
primitive recursive term operations coexist with a proof apparatus subject to
G2. [Simpson][simpson], Section 6.6, printed pages 119-120, explicitly allows
T = PRA and states Theorem 6.6.7:

```text
If T is consistent, T does not prove Con(T).
```

The consistency sentence uses the ordinary arithmetized proof predicate from
Section 6.5. This supplies a direct additional reference for the user's
earlier request about PRA.

## A total trace checker already supports universal descriptions

The separating example can be derived without assuming that execution is
universal. Fix an ordinary encoding of machines, inputs, configurations, and
finite sequences. Define

```text
Trace(e, x, t) =
  t encodes a finite sequence of configurations;
  its first configuration is the initial configuration of machine e on x;
  every successive pair obeys e's transition table;
  its last configuration is halting.
```

Checking a supplied sequence uses finite decoding, bounded scans, and local
transition checks. Under standard effective sequence encodings these
operations are primitive recursive. The checker always terminates, including
when the proposed trace is malformed. It does not search for a trace.

Nevertheless, in a logic that can express this relation and quantify over
arbitrarily large trace codes, the sentence

```text
H(e, x) := exists t. Trace(e, x, t)
```

has exactly the following standard-model meaning:

```text
H(e, x) is true  iff  machine e halts on input x.
```

For the forward direction, decode the asserted trace. Its initial, transition,
and final checks exhibit an actual finite halting computation. For the reverse
direction, encode the actual halting computation as the witness t.

To turn this description into an executable universal partial evaluator, one
can search over t until a valid trace is found and then extract the output.
That search need not terminate. The description uses an unbounded logical
existential; the evaluator uses potentially unbounded computation.

Thus a program class can compute the total predicate Trace while failing to
admit the corresponding unrestricted search program. Adding an existential
assertion to a specification is not itself adding an executable search
operator. In a constructive calculus, obtaining a term for that assertion
would additionally require a derivation supplying its witness.

## A precise obstruction to decidable theoremhood

**Proposition.** Suppose S satisfies all four conditions:

1. Its proofs can be effectively enumerated.
2. There is an effective translation (e, x) to H(e, x) with the meaning above.
3. If e halts on x, then S proves H(e, x).
4. If S proves H(e, x), then e halts on x.

Then the theorem set of S is computably enumerable complete under computable
many-one reductions.

**Proof.** Conditions 3 and 4 give

```text
e halts on x  iff  S proves H(e, x).
```

Condition 2 makes this equivalence an effective reduction of the halting
problem to theoremhood. Conversely, given a sentence A, a machine can
enumerate S-proofs and halt exactly when it finds a proof of A. Condition 1
makes this a reduction of theoremhood to halting. This proves the claim.

Condition 4 is soundness for this sentence class; consistency alone is not a
replacement for it. Nor does semantic expressibility by itself establish
Condition 3. A theory with decidable theoremhood could lack the required
halting-witness proofs even when its language has a richer intended semantics.

There is a separate purely semantic consequence: if the full truth set of a
structure is decidable, it cannot admit such an effective translation of all
halting assertions. Deciding the translated sentence would decide halting.

## Where Willard and Pakhomov sit

[Willard 2011][willard], Definition 3.4, builds numeral representation, bounded
graphs of addition and multiplication, and completeness for true existential
bounded-matrix sentences into a generic configuration. Lemma 3.6 describes an
effective translation from conventional bounded arithmetic to the native
bounded language, preserving standard-model truth. The lemma's source
justification is a sketch referring to Paris-Dimitracopoulos.

Applying this translation to an ordinary existential arithmetization of a
halting trace supplies the representation required above. The base's
completeness supplies the witness proofs. Consequently every sound effective
instance retaining these requirements satisfies the proposition. This is a
derived consequence of the framework, not a claim about a newly specified
execution semantics for Willard's systems.

The lack of an internally proved uniform multiplication-totality theorem does
not remove these finite-computation descriptions. A weaker system lying below
this representational boundary must change the relevant framework requirements,
not merely reduce its selection of native uniform totality theorems.

[Pakhomov][pakhomov] provides an independent positive benchmark. Theorem 5.4
states that H proves NAT(Con_pred(H_<omega)); hence its extension H_<omega
proves its specified Hilbert consistency. Appendix A proves mutual
interpretability of H_<omega and Robinson's R. These are distinct claims:
mutual interpretability does not automatically transfer self-consistency to
R's own native proof predicate.

Here is an additional derivation locating R on the representation axis. Its
axioms, displayed in Appendix A, prove every correct ground addition and
multiplication equation, distinguish different numerals, and reduce x <= n
for a fixed numeral n to a finite list of numeral cases. They also provide
x <= n or n <= x, which together with those finite lists decides comparisons
of numerals. An external induction on a bounded formula with fixed numeral
parameters therefore supplies a proof of that formula or its negation
according to its standard truth value: compute its closed terms and expand
each bounded quantifier into its finitely many cases. No induction scheme
inside R is being used.

In particular, every true existential bounded-matrix arithmetic sentence has
a proof in R: substitute its actual numeral witnesses, prove the resulting
bounded instance by the preceding argument, and introduce the existentials.
Apply this to the usual existential arithmetization of halting. R can thus
describe arbitrary finite computations and prove each genuine halting
instance. Its interpretation in H_<omega transfers these formulas and proofs;
in the intended hereditarily finite set model, the interpreted natural
numbers and arithmetic have their standard meanings.

Pakhomov's system is therefore a weak self-consistency example on the
interpretability scale, but remains above the universal finite-computation
representation boundary. Its failure to prove uniform totality of its native
ordinal successor does not reverse that conclusion. No comparison here claims
it is strictly weaker than every Willard system.

## The separate limit on certifying total programs

**Proposition.** No effective certificate system that is sound for totality
can certify an implementation of every total computable function from natural
numbers to natural numbers.

**Proof.** Suppose it could. Enumerate accepted program/certificate pairs,
giving programs e_0, e_1, ... . The enumeration is infinite under this
supposition, since infinitely many distinct numerical functions have accepted
implementations. Repeated programs do not affect the argument. Define

```text
g(n):
    enumerate accepted pairs until the n-th pair is reached;
    run its program e_n on input n;
    return its output plus 1.
```

Both stages terminate: the required accepted pair exists, and soundness makes
its program total. Hence g is a total computable function. If some e_k
computed g, then

```text
g(k) = e_k(k) + 1 = g(k) + 1,
```

a contradiction. Therefore some total computable functions have no accepted
implementation.

This is an external diagonal argument. It does not assert that g is
expressible in a chosen total executable fragment or that its totality is
provable in S. Self-consistency leaves the argument intact because it does not
remove the hypotheses of effective certificates and sound totality claims.

## What lies below the representation boundary remains a separate target

Presburger arithmetic is a clear comparison for limited representation.
[Zapryagaev and Pakhomov][presburger], introduction, distinguish its
decidability from the syntax needed to formulate the usual consistency
statements. On page 2 they explain the failure of the relevant internal
syntax coding. Its external decision procedure does not supply an internal
self-consistency theorem for its ordinary full proof apparatus.

[Khodier, Schaeffer, and Shallit][buchi], Section 4.2, use "self-verifying" for
predicates whose automata can be validated by suitable recurrence conditions.
This is useful verification within a decidable setting. It supplies no result
that the entire underlying arithmetic proves its own raw-checker consistency.
The terminology should not be substituted for the requested property.

For a self-justifying theory below universal finite-computation representation,
the desired package would be:

```text
S lacks an effective adequate representation of arbitrary machine traces;
S still expresses the actual acceptance relation of its own proof checker;
S proves that this checker accepts no proof of False;
the quantifier covers every candidate certificate, including its own proof;
an external argument proves consistency of the full S.
```

Decidable theoremhood would be an additional particularly useful property.
It is stronger than merely failing the halting-completeness conditions above.
Failure of one universal representability condition does not by itself imply
decidability, just as undecidability by itself does not establish universal
trace representation.

The cited results establish neither an impossibility theorem for this package
nor a natural decidable example of it. The execution restriction is compatible
with an existing SJAS; lowering the strength of that same self-justifying
proof apparatus is the unresolved part of this review. A claim of a weakest
system would additionally require a fixed ordering, such as interpretability,
and a specified adequacy criterion for its proof representation.

## Source and validation record

- Meyer and Ritchie (1967), *The complexity of loop programs*, printed pages
  465-466, PDF pages 1-2: execution discipline and function-class statement
  checked against rendered page images.
- Simpson, *Mathematical Logic* (1998-2005 notes), Section 6.6, printed pages
  119-120, PDF pages 120-121: the PRA instance, consistency predicate, and
  Theorem 6.6.7 checked against page images. The theorem has a displayed proof
  using the derivability lemmas; the promised detailed formalization of Lemma
  6.5.3 is omitted in Section 6.7 of this draft. It is cited for its explicit
  theorem statement, without claiming to have replayed a complete formalization.
- Willard (2011), Definition 3.4 and Lemma 3.6, printed pages 9-10, PDF pages
  10-11: rendered and inspected. The imported translation argument retains
  `sketch` status. The conditional halting reduction above is a full paper
  derivation from its stated hypotheses.
- Pakhomov (2019), construction on page 3, Theorem 5.4 on page 23, and Appendix
  A on page 24: rendered and inspected. The theorem is proved in the source;
  this review has not independently replayed its entire proof. The R
  bounded-instance argument above is an explicit external derivation from the
  displayed axioms.
- Zapryagaev and Pakhomov (2018 version), introduction, especially PDF page 2:
  rendered and inspected for the limitation of the usual consistency coding.
- Khodier, Schaeffer, and Shallit (2025 preprint), Sections 2-4.2: HTML checked
  to distinguish automaton verification from arithmetic self-consistency.

The finite-trace separation, totality-certificate diagonal argument, and
conditional theoremhood reduction are paper proofs. No finite experiment or
repository test is claimed to validate their universal conclusions.

[loop]: https://people.csail.mit.edu/meyer/meyer-ritchie.pdf
[simpson]: https://people.math.wisc.edu/~awmille1/old/m571-08/simpson.pdf
[willard]: https://arxiv.org/pdf/1108.6330
[pakhomov]: https://arxiv.org/pdf/1907.00877
[presburger]: https://arxiv.org/pdf/1709.07341
[buchi]: https://arxiv.org/html/2507.19717v1
