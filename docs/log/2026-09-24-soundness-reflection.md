# Soundness reflection and the limits of self-reflection

Date: 2026-09-24.

Question: Is there a reflection principle expressing "I am sound," analogous
to an internal consistency statement?

Yes. Local and uniform reflection express soundness as schemas. Global
reflection gives the direct single-statement formulation when an appropriate
truth predicate is available. Expressibility and internal provability are
separate questions.

## The three formulations

Fix a theory S, its actual proof apparatus, and an intended standard
interpretation. Write `Prov_S(a)` for existence of an S-proof of the sentence
with code a. For every sentence A, local reflection has an instance

```text
Prov_S(code(A)) -> A.
```

This is a schema: each A supplies a separate sentence in the original
language. Assuming the proof predicate has its intended standard meaning,
all these instances are true exactly when every S-theorem is true. A code
variable cannot simply replace A on the right: arithmetic variables denote
numbers, not formulas to be evaluated.

For a formula A(x), uniform reflection instead has the instance

```text
forall n. (Prov_S(code(A(numeral(n)))) -> A(n)).
```

Here the code on the left is computed by substituting the numeral for n into
A. Each formula still supplies an instance of a schema. The uniform and
local schemas differ in deductive strength, despite both expressing external
soundness when evaluated in the standard model. See Artemov and Beklemishev,
printed/PDF page 31, for both definitions.

With an adequate truth predicate for a designated language L, global
reflection is the single formula

```text
forall a.
  ((Sentence_L(a) and Prov_S(a)) -> True_L(a)).
```

Its intended meaning is that every L-sentence proved by S is true. To cover
all of S's theorem language, L must include that entire language. Tarski's
undefinability theorem prevents an arithmetical predicate from defining full
arithmetical truth in the standard natural numbers. An appropriate richer
language can express truth for the original arithmetic language; a truth
predicate for that original language does not automatically cover the
expanded language containing the predicate itself. See Harrison, printed
page 18 / PDF page 19.

This is a limitation on the uniform full-truth formulation. It is not a
claim that no particular finitely axiomatized theory can have its soundness
characterized by a special arithmetical condition, for example the truth of
its finitely many axioms under a sound calculus.

For a fixed arithmetical complexity class Gamma, suitable arithmetic admits
partial truth predicates. Replacing `Sentence_L` and `True_L` by
`Sentence_Gamma` and `True_Gamma` gives a single restricted soundness
statement. One must check the available coding and correctness results in
any especially weak SJAS before importing this construction internally.

## Consistency and the internal obstruction

Setting A to False in local reflection gives

```text
Prov_S(code(False)) -> False,
```

equivalently `not Prov_S(code(False))`: ordinary consistency for that proof
predicate. Willard's selected Level(1) self-consistency statements have their
own paired-proof formulation; that should not be silently identified with
every conventional consistency formula inside a weak theory.

For conventional sufficiently strong arithmetic theories with the required
diagonalization and derivability conditions, Lob's theorem states

```text
if S proves (Prov_S(code(A)) -> A), then S proves A.
```

Thus a consistent such S cannot prove all its own reflection instances:
the False instance already invokes the second incompleteness obstruction.
Reflection instances for sentences S already proves are unproblematic.
Harrison's discussion on printed page 18 states this result. The usual
derivability hypotheses cannot be assumed wholesale for Willard's SJAS.

A separate direct argument clarifies the obstruction without using the
full Hilbert-Bernays-Lob conditions. This is our derivation, under the
following explicit hypotheses: S has ordinary propositional reasoning, a
sentence G with `S proves G <-> not Prov_S(code(G))`, and theoremwise
internalization (`S proves A` implies `S proves Prov_S(code(A))`). Put
`P = Prov_S(code(G))`. If S also proved reflection at G, then:

1. Reflection gives `P -> G`; the fixed point gives `G -> not P`.
2. Hence S proves `P -> not P`, and therefore `not P`.
3. The other fixed-point direction yields an S-proof of G.
4. Theoremwise internalization yields an S-proof of P.
5. S is inconsistent, since it proves P and its negation.

This argument requires neither boxed modus ponens nor positive
introspection, but it does require the stated fixed point, internalization,
and propositional resources. It is not an unconditional assertion about
every conceivable formal system.

If S is externally sound, adjoining its reflection instances produces a
sound extension `S' = S + Rfn(S)` under sound inference rules: the newly added
axioms are true. Those axioms refer to proofs in S. They do not automatically
reflect the stronger proof predicate of S'. This explains why proving a
predecessor's soundness differs from proving one's own.

## The relevant Willard result

Willard 2011, Theorem 6.12, supplies an actual restricted translated
reflection result. For an EA-stable configuration, let
`S = B + SelfCons^1(B)`. For every permitted Pi_1 sentence A, the construction
provides a translated sentence A^tau and an S-proof of

```text
forall p. (Prf_S(code(A), p) -> A^tau).
```

In the proof, A^tau expresses that B has no proof of not A. The translation
is equivalent to A in the standard model. However, the equivalence is not
available internally for all such sentences; footnote 15 explicitly
identifies that distinction. Consequently this theorem does not furnish
ordinary reflection with A itself as the consequent. Remark 6.14 discusses
the obstruction posed by the Godel sentence.

This is compatible with the preceding
[external soundness argument](2026-09-24-soundness-of-willard-sjas.md).
That argument uses the true, Sigma_1-complete base and the shape of the
added axioms in the metatheory. It does not establish that S can express and
prove all those semantic facts about itself.

## Clarification: which equivalences fail internally?

Follow-up: 2026-09-25.

The equivalences at issue are `A <-> A^tau`, where the translation used in
Theorem 6.12 is, with coding and the deduction-method subscript suppressed,

```text
A^tau := not Prov_B(not A).
```

The base B here is the original B^xi, not the enlarged self-justifying system
S. Thus the proposed equivalence says that A holds exactly when B has no
proof of its negation.

The external equivalence has two distinct reasons. If A is true, B's soundness
excludes a B-proof of not A. If A is false, its negation is a true Sigma_1
sentence, so B's Sigma_1 completeness supplies such a proof. These external
facts do not automatically provide their formalized counterparts in S.

It is not claimed that every individual equivalence is unprovable. Under
the theoremwise internalization hypothesis stated above, if S proves A,
translated reflection gives an S-proof of A^tau, and propositional reasoning
then proves their equivalence.

A specific obstructed instance is the Godel sentence G from the direct
argument above. Under that argument's fixed-point, internalization, and
propositional hypotheses, consistency of S implies

```text
S does not prove (G^tau -> G),
and hence S does not prove (G <-> G^tau).
```

Indeed, S already proves `Prov_S(G) -> G^tau` by translated reflection.
An S-proof of `G^tau -> G` would therefore give ordinary reflection at G,
which the displayed fixed-point argument showed inconsistent with those
hypotheses. This identifies the failing reverse implication without claiming
that both directions fail for every A. Classically, the missing implication
`not Prov_B(not G) -> G` is equivalent to `not G -> Prov_B(not G)`:
the relevant instance of formalized Sigma_1 completeness for the base B.

Example 6.7, Theorem 6.12, Remark 6.14, and footnote 15 supply the translation
and the Godel-sentence motivation. The explicit implication-level conclusion
is the derivation above, with its hypotheses retained.

## Clarification: independence of Godel sentences

Follow-up: 2026-09-25.

Independence of G from T means that T proves neither G nor not G. For an
effectively axiomatized arithmetic theory with the ordinary coding and
fixed-point resources, distinguish the following results:

- Consistency implies that the ordinary Godel sentence G is unprovable and
  true in the standard model. It does not in general exclude a proof of not G.
- Sigma_1 soundness suffices to exclude a proof of not G, and hence gives
  independence. Full soundness suffices as well. Godel's original argument
  used omega-consistency, which also suffices.
- Rosser modifies the self-referential sentence. For effectively axiomatized
  extensions of Robinson arithmetic, consistency alone makes the resulting
  Rosser sentence R independent. This is a statement about R, not an
  improvement of the independence assumptions for the unchanged ordinary G.

The ordinary-sentence argument separates cleanly. If T proved G, it could
verify that particular proof and prove `Prov_T(G)`, while its fixed point
gives `not Prov_T(G)`, violating consistency. Thus no actual T-proof of G
exists. The standard Godel construction then makes G true. A proof of not G
would, by the fixed point, give a proof of the false Sigma_1 assertion
`Prov_T(G)`. Sigma_1 soundness rules this out; mere consistency does not.

Consequently, the sound Willard instances discussed here have independent
ordinary Godel sentences whenever the stated fixed-point/proof-representation
construction is available. The preceding obstruction to reflection at G
needed only the consistency-based unprovability argument, not the stronger
claim that not G is also unprovable.

Harrison, printed pages 16-17 / PDF pages 17-18, supplies this distinction;
both pages were rendered and inspected. For an explicit formal analysis of
the assumptions, see Popescu and Traytel,
[Distilling the Requirements of Godel's Incompleteness Theorems with a Proof Assistant](https://link.springer.com/article/10.1007/s10817-021-09599-8),
Sections 6.2-6.4. Its abstract setting also makes clear that Rosser's change
requires additional ordering and proof-representation resources; it should
not be imported into arbitrary unconventional logics from consistency alone.

## Sources and verification

- Sergei Artemov and Lev Beklemishev,
  [Provability Logic](https://sartemov.ws.gc.cuny.edu/files/2012/10/Artemov-Beklemishev.-Provability-logic.pdf),
  printed/PDF page 31: local and uniform reflection, complexity restrictions,
  and partial truth predicates. Its equivalences over EA are not imported
  without checking their hypotheses for the selected SJAS.
- John Harrison,
  [Metatheory and Reflection in Theorem Proving: A Survey and Critique](https://www.cl.cam.ac.uk/~jrh13/papers/reflect.pdf),
  printed page 18 / PDF page 19: global/local/uniform reflection, Tarski,
  consistency as an instance, Lob, and successive proof predicates.
- Dan Willard,
  [A Detailed Examination of Methods for Unifying, Simplifying and Extending Several Results About Self-Justifying Logics](https://arxiv.org/pdf/1108.6330),
  printed pages 26-27 / PDF pages 27-28: Theorem 6.12, Remark 6.14, and
  footnote 15. Also available in the
  [local paper](../../nachlass/papers/willard2011_self_justifying_logics_arxiv_1108.6330.pdf).

All cited pages were rendered and visually inspected. The direct fixed-point
derivation above is a paper proof under explicit hypotheses, not a
mechanized theorem or a new implementation of reflection.
