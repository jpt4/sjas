# A categorical comparison of AU incompleteness, Beklemishev-Shamkanov, and Willard SJAS

*R6 of the Refinement stage. Charter:
[`ADR-0004-lawvere-sjas-translation.md`](ADR-0004-lawvere-sjas-translation.md).*

> **Status and scope.** This is a comparison construction, not a claim that
> Beklemishev-Shamkanov's calculus or Willard's arithmetics are arithmetic
> universes. It identifies the categorical data used by the arithmetic-universe
> proof and then presents the other systems in the weakest common language that
> preserves their failure mechanisms. Claims about the three source corpora are
> page-image verified in [`VERIFICATION.md`](VERIFICATION.md).

---

## 0. Answer

The comparison needs **one more coordinate than the three
Hilbert-Bernays-Löb (HBL) conditions**. For the usual formalized-G2 argument,
the relevant package is

1. a selected Gödel/Löb fixed point `G <-> not □G`;
2. the three HBL structure maps for the provability operation `□`; and
3. a diagonal on boxed hypotheses,
   `copy_A : □A -> □A tensor □A`, expressing `□`-contraction.

Willard's self-justifying construction uses a second, logically distinct
diagonal construction: a sentence `H` that says that the theory obtained by
adjoining `H` is consistent. Here `H` is the Group-3 axiom. The G2 fixed point
`G` and the self-consistency fixed point `H` must not be identified merely
because both are obtained by fixed-numeral substitution.

The initial arithmetic universe has the whole G2 package: finite products
supply the diagonal, van Dijk-Oldenziel Propositions 6.5, 6.6 and 6.8 supply
the HBL maps, and Lemma 6.12 supplies fixed points.

Beklemishev-Shamkanov's example `S` does **not** fail one of the three HBL
conditions. It satisfies their versions L1-L3 and has every modalized fixed
point needed here. What it lacks is the boxed diagonal. Categorically, its
Lindenbaum construction is symmetric monoidal rather than cartesian; `□A`
cannot in general be copied. Consequently the formalized-G2 arrow

`□(□bottom -o bottom) -> □bottom`

does not exist. But neither does a global proof of consistency
`I -> (□bottom -o bottom)`. Thus `S` blocks the G2 derivation without proving
its own consistency. The system is nevertheless externally nondegenerate: if
it were inconsistent in B-S's Definition 2.1 sense, its consequence rules and
cut admissibility would make the sequent of Proposition 4.1 provable.

Willard's Type-A `IS(A)` lies on the other axis. Its ordinary classical logic
has cartesian contraction, but its weak arithmetic cannot internalize the
**uniform proof-code composition** required by HBL condition (2). In the
syntactic proof doctrine there is no internal entailment

`□Phi and □(Phi -> Psi) -> □Psi`

uniform in the two proof witnesses, although every fixed-proof-code instance
can be verified. The same pointwise/uniform split supplies selected diagonal
instances without proving total substitution: Willard 2001, Theorem A.1
footnote 16, gives the particular `G <-> not □G` needed by the HBL argument
under that theorem's `Pi^-_1`-strength hypothesis, while the 1993 construction
uses a different fixed instance to form Group-3. Group-3 is a global
self-consistency point, and Willard separately proves external consistency.
B-S's `S` also has external consistency; what it lacks is the **positive
internal point**.

In short:

| System | G2 fixed point | HBL maps | Boxed diagonal | Internal self-consistency | Externally consistent |
| --- | --- | --- | --- | --- | --- |
| Joyal / van Dijk-Oldenziel AU | every `T : P1' -> P1'` | all three | yes, cartesian | no point for the internal copy's consistency proposition if `U_0` is consistent | assumed for `U_0` in G2 |
| Beklemishev-Shamkanov `S` | every modalized formula, including the needed `G` | **all three** | **no** | **no** | **yes**, as forced by Proposition 4.1 |
| Willard Type-A `IS(A)` | selected `G` under Theorem A.1's strength hypothesis; Group-3 is separate | **HBL (2) fails uniformly** | yes | **yes** | **yes**, under Proposition 1's hypotheses (`full`) |

This is the requested distinction: B-S exhibit an externally consistent system
with **non-derivability of formalized G2**, but not a counterexample to
non-formalized G2; Willard establishes a **consistent inhabitant of the
self-consistency proposition**.

---

## 1. The common comparison object: a coded modal proof doctrine

Neither an arithmetic universe alone nor a bare modal algebra records all the
distinctions at issue. We therefore use the following deliberately small
interface. This is the Refinement's comparison construction, not terminology
claimed from any of the three sources.

### 1.1 Structural category

Let `K` be a category, or a representable symmetric multicategory, of formula
contexts and derivations. Its tensor `tensor` represents context combination,
its unit `I` represents the empty context, and an internal implication
`A -o B`, when present, satisfies

`K(Gamma tensor A, B) ~= K(Gamma, A -o B)`.

The notation `A -o B` below is schematic at exactly one important point. When
the implication is representable, as in the B-S consequence category, it is an
object satisfying this adjunction. In the AU presentation it instead denotes
van Dijk-Oldenziel's implication **between judgements**, interpreted in the
appropriate classifying AU (Definitions 6.1-6.2). No exponential object in the
base AU is being assumed. The theorem below needs the associated introduction,
evaluation, composition, and contraposition steps, not cartesian closure.

Write `not A := A -o bottom` throughout this comparison.

If `tensor` is a categorical product, every object has a diagonal

`delta_A : A -> A tensor A`.

Proof-theoretically, precomposition with `delta_A` is contraction. In a merely
monoidal category no such diagonal is automatic.

### 1.2 Provability data

For each coded sentence `A`, let `Prf_A` be its object or predicate of proof
witnesses, and let

`□A := exists p. Prf_A(p)`.

The three HBL conditions become the following structure.

| Name | Categorical/proof-doctrine form | Classical condition |
| --- | --- | --- |
| **Nec** | global proofs of `A` lift to global proofs of `□A` | if `|- A`, then `|- □A` |
| **BoxMP** | `m_A,B : □(A -o B) tensor □A -> □B` | `□(A -> B) -> (□A -> □B)` |
| **Four** | `four_A : □A -> □□A` | `□A -> □□A` |

Across nonclassical logics, these three maps are not enough. The additional
structural datum isolated by Beklemishev-Shamkanov is

`BoxContr_A : □A -> □A tensor □A`.

In a cartesian doctrine this is just `delta_{□A}` and is invisible as a separate
assumption. In an affine, linear, or other substructural doctrine it is real
extra structure.

### 1.3 Two fixed-point roles, and consistency points

A fixed point is an object or proposition `X` equipped with derivations both
ways between `X` and `E(X)`. Three strengths must not be conflated:

- **universal**: every endomorphism in a stated class has a fixed point;
- **modalized**: every syntactically modalized formula has a selected fixed
  point;
- **selected**: one specified endomorphism has a fixed point.

Two selected endomorphisms occur in this comparison:

- the **G2 diagonal** selects `G <-> not □G`; this is the fixed point consumed
  by Theorem 1.1;
- a **self-consistency diagonal** selects a sentence `H` with
  `H <-> Con(B + H,D)`, where the proof predicate itself depends on the theory
  obtained by adjoining `H` to the non-self-referential base `B`.

The first is an ingredient of the limitative argument. The second is how a
self-referential consistency assertion is formed; adjoining `H` then turns it
into a consistency point. Either selected instance can exist without an
internally total substitution operation, and neither instance by itself implies
the other.

A *consistency point* is a global derivation

`con : I -> Con`, where `Con := □bottom -o bottom`, with `-o` read in the
proof-doctrine sense just specified.

This is only the internal half of self-justification. The other half is the
external, metatheoretic assertion that the theory is consistent. An inconsistent
theory can of course inhabit every proposition, including `Con`.

### 1.4 The comparison theorem

**Theorem 1.1 (categorical HBL-G2 package).** Suppose the doctrine has a
selected fixed point `G <-> not □G`, `Nec`, `BoxMP`, `Four`, the instance
`BoxContr_G`, and enough implication rules to perform the usual propositional
packaging. Then the formalized-G2 arrow `□Con -> □bottom` is derivable; under
external consistency there is no consistency point.

*Proof.* Let `G <-> not □G` be the fixed point. Apply `Nec` and `BoxMP` to the
forward direction `G -> (□G -> bottom)`. This gives

`□G -> □(□G -> bottom)`.

`Four` gives `□G -> □□G`. To apply `BoxMP` once more, now with `A = □G` and
`B = bottom`, the same input `□G` must feed both arrows. This is the one
critical use of

`BoxContr_G : □G -> □G tensor □G`.

The resulting composite is `□G -> □bottom`. Hence

`Con -> not □G`, and therefore `Con -> G` by the reverse fixed-point arrow. If
we apply `Nec` and `BoxMP` to `Con -> G`, we get `□Con -> □G`; composition
therefore gives the advertised `□Con -> □bottom`. If `Con` had a global proof,
`Nec` would supply `□Con`, formalized G2 would supply `□bottom`, and `Con`
itself would yield `bottom`. This is the ordinary HBL proof with the normally
implicit cartesian diagonal made explicit. Beklemishev-Shamkanov Proposition
3.8 identifies the same contraction use in their abstract proof. ∎

The theorem is a **sufficient package**, not a biconditional. The displayed
proof consumes only the named instances--in particular `BoxContr_G`, not a
diagonal at every object. If a needed instance is unavailable, this derivation
is blocked; mere absence of the general family does not by itself prove that
the conclusion is underivable, manufacture a consistency point, or prove
external consistency.

---

## 2. Joyal / van Dijk-Oldenziel: the full cartesian package

### 2.1 Their actual AU construction

Let `U_0` be the initial arithmetic universe and `U'_0` the externalization of
its internal initial arithmetic-universe object. Van Dijk-Oldenziel construct

- an interpretation functor `R : U_0 -> U'_0`;
- a global-sections functor `Gamma : U'_0 -> U_0`; and
- the provability operation `□ := Gamma R`.

A proposition is a monomorphism `phi -> 1`. Because an arithmetic universe is a
list-arithmetic pretopos, its structural tensor is a finite product. Every
proposition and every boxed proposition therefore has a diagonal.

The paper stresses that an arithmetic universe need not be cartesian closed.
Section 6 consequently treats `phi |- psi` as an inclusion of subobjects and
interprets implications between such judgements in a classifying arithmetic
universe. The comparison here follows that construction; it does not pretend
that `U_0` has an ordinary exponential implication.

### 2.2 HBL as AU arrows

The source's propositions instantiate the common interface exactly as follows.

| Common datum | van Dijk-Oldenziel | Construction |
| --- | --- | --- |
| `Nec` | Proposition 6.5 | `U |= phi` implies `U |= □phi`, from the unit-like map `phi -> Gamma R(phi)` of Lemma 5.15 |
| `Four` | Proposition 6.6 | `U |= □phi |- □□phi`, by applying `□ = Gamma R` |
| `BoxMP` | Proposition 6.8 | internal evaluation `Hom(phi',psi') x Hom(1',phi') -> Hom(1',psi')` in the classifying AU |
| ordinary MP | Propositions 6.7, 6.9, 6.10 | composition and packaging of judgements |
| fixed point | Lemma 6.12 | every `T : P1' -> P1'` has a global fixed point |
| contraction | finite products in the AU | `delta_X : X -> X x X` |

Thus Theorem 6.13 derives Löb in §6, which the authors explicitly introduce as
a sketch. The direct G2 result is Theorem 5.20; equivalently, the
false/consistency instance of the Löb package rules out an internal proof of
the consistency of `U'_0` when `U_0` is consistent.

The load-bearing map for comparison with the other systems is Proposition 6.8's
evaluation. It is a single internal, uniform arrow. The existence of its value
at every externally named pair would be weaker.

---

## 3. Beklemishev-Shamkanov: HBL without the boxed diagonal

### 3.1 The APS as a thin category

Beklemishev-Shamkanov Definition 2.1 starts with a preorder

`S = (L_S, <=_S, top, bottom)`.

View it as a thin category `L(S)`: its objects are sentences (or their
deductive-equivalence classes), and there is a unique arrow `A -> B` exactly
when `A <=_S B`. Definition 2.3 then adds the object operations `□`
(provability) and `⊠` (refutability), subject to APS conditions C1-C4. This is
already an order-enriched categorical presentation of their abstract
provability structure.

### 3.2 The consequence calculus as a monoidal category

Definitions 3.1-3.2 refine the preorder to multiset contexts. After quotienting
by deductive equivalence, set

`K_S(A,B) = {*} if A |-S B, and empty otherwise`.

Multiset union is represented by their conjunction `tensor`; `top` is the unit;
and I3 gives the closure adjunction

`K_S(Gamma tensor A, B) ~= K_S(Gamma, A -> B)`.

Exchange is built in because contexts are multisets. Cut is categorical
composition: for the concrete system `S`, Theorem 5 proves cut admissible, so
this presentation really is compositional. Lemma 5.1 also proves weakening
admissible, so its monoidal unit is terminal: categorically the concrete system
is symmetric affine monoidal closed. What is absent is neither composition nor
weakening, but the diagonal that would make the tensor cartesian.

Their three Löb conditions are precisely:

- L1: `□(A -> B) -> (□A -> □B)` (`BoxMP`);
- L2: `□A -> □□A` (`Four`);
- L3: global proofs lift under `□` (`Nec`).

The modal rule of the concrete calculus supplies these conditions. Its
fixed-point constructor supplies `fp x.A ~= A(fp x.A)` for every formula
modalized in `x`, so in particular it supplies the `G <-> not □G` instance used
in Theorem 1.1. Proposition 4.3 also gives infinitely many inequivalent
Gödelian and Henkinian fixed points. Here *Gödelian* is B-S's Definition 2.5
term for the Jeroslow-shaped equation `p <-> ⊠p`; it should not be confused
with the particular unprovability fixed point denoted `G` in this comparison.

### 3.3 The precise missing morphism

Contraction would be a family

`delta_A : A -> A tensor A`.

Restricted `□`-contraction is only the family

`delta^□_A : □A -> □A tensor □A`.

Beklemishev-Shamkanov Remark 3.9 shows that this restricted family, together
with correspondingly weakened APS assumptions, is already enough for their G2
theorem. Their concrete system deliberately has no such family. In sequent
form, one cannot uniformly pass from

`Gamma, □A, □A |- B`

to

`Gamma, □A |- B`.

This is why the concrete `S` can satisfy L1-L3 and have fixed points while
Proposition 4.1 states that the formalized-G2 arrow is absent (the paper calls
this easy from cut-freeness and does not display a proof):

`□(□bottom -> bottom) -/-> □bottom`.

This also explains why an affine surface syntax is not enough: B-S note that an
affine Peano arithmetic can still admit `□`-contraction. The categorical test is
the existence of `delta^□`, not the absence of an unrestricted contraction rule.

### 3.4 Why this is not self-justification

Their Section 6 says explicitly that the example is not a counterexample to
non-formalized G2, because

`=> not □bottom`

is not provable. Categorically, `K_S(I, Con)` is empty.

This does **not** mean that `S` is externally inconsistent. In fact, the result
of Proposition 4.1 forces consistency in the paper's Definition 2.1 sense. If
`top <=_S bottom`, condition I4 would give `|-S bottom`. For the antecedent
`F := □(□bottom -> bottom)`, the bottom initial sequent gives
`F,bottom |-S □bottom`; I2--implemented for the concrete calculus by Theorem
5's admissible cut--would then give `F |-S □bottom`, contrary to Proposition
4.1. The example therefore occupies the cell

`fixed points + HBL + no BoxContr + no Con point + external consistency`.

It proves that the HBL triple is insufficient once the ambient structural
category ceases to be cartesian. It provides a consistent system that does not
assert its own consistency, not a self-justifying system.

---

## 4. Willard: cartesian logic without internal uniform proof composition

### 4.1 Two levels that must be kept separate

Willard's generalized arithmetic is a pair `(alpha,D)` of an axiom basis and a
deductive apparatus. Its categorical presentation must distinguish:

1. the external category in which mathematicians can manipulate finite proof
   codes; and
2. the internal syntactic category of operations and entailments whose
   totality the weak theory `alpha` itself can prove.

Let `C_alpha` be the cartesian syntactic category of `alpha`: objects are
definable contexts and arrows are equivalence classes of functional relations
whose totality and functionality `alpha` proves. Its predicate doctrine

`P_alpha : C_alpha^op -> Pos`

sends a context to its preorder of formulas modulo provable equivalence;
reindexing is substitution. Finite meets and existential quantification give
the usual first-order hyperdoctrine structure. The proof relation is a
predicate

`Prf_{alpha,D} in P_alpha(Sent x N)`.

For a fixed sentence `Phi`, pull it back along its code to obtain the proof
predicate `P_Phi` over `N`, and put

`□_D Phi := exists p. Prf_{alpha,D}(code(Phi),p)` in `P_alpha(1)`.

The terminal fiber `P_alpha(1)` is a genuine thin category: an arrow is an
implication proved by `alpha`, and proved implications compose.
This repairs the earlier draft's invalid “wide class of affordable arrows”,
which was expressly not closed under composition and hence was not a category.

The ordinary classical first-order logic of `IS(A)` supplies cartesian context
management, so contraction and the diagonals `delta_X` are present. Willard
does not use the B-S escape hatch.

### 4.2 HBL (2) as uniform evaluation

For fixed sentences `Phi,Psi`, HBL (2) has the following proof-witness form in
the fiber over `N x N`:

```
Prf(code(Phi),x) and Prf(code(Phi -> Psi),y)
  -> exists z. Prf(code(Psi),z).
```

Its universal closure is equivalent, by ordinary first-order quantifier rules,
to the terminal-fiber `BoxMP` entailment

`□Phi and □(Phi -> Psi) -> □Psi`.

A stronger but familiar witness for this entailment would be a proof-combiner
map

`comp_Phi,Psi : P_Phi x P_(Phi -> Psi) -> P_Psi`.

Van Dijk-Oldenziel Proposition 6.8 obtains exactly the analogous uniform
operation from evaluation on internal Hom objects.

Willard's 1993 technical report, printed p. 12, identifies the breach for
Type-A `IS(A)`: the system can prove the required composition statement only
when the proof-code variables `x` and `y` are fixed constants. In categorical
terms, pulling the witness entailment back along a named point

`<x-bar,y-bar> : 1 -> N x N`

can yield a proved instance, while there is no entailment uniformly over the
whole product. “Every named instance” does not assemble into the uniform
evaluation morphism. This is analogous to, but not an identification with, the
local-versus-global existence warning in van Dijk-Oldenziel Remark 5.12.

The missing internal arrow is not the external nonexistence of proof
composition. A metatheorist can combine tableaux derivations and appeal to cut
elimination; the failure is that the weak arithmetic does not prove one
uniform, total proof-code relation over variable `x` and `y`. The source's
page-12 passage identifies that failure. The surrounding design--weak
arithmetic together with a cut-free apparatus whose composition is supplied
metatheoretically by cut elimination--explains why fixed inputs and a uniform
internal transformation come apart. This comparison does not claim an
independent complexity lower-bound theorem.

Accordingly, for the Type-A tableaux line the documented failure is the
uniform witness entailment, and hence

`BoxMP_Phi,Psi` (HBL condition (2)),

not contraction. This document does not claim that every Willard configuration
fails the same condition. The source corpus identifies condition (2) for
`IS(A)`; for the separate Hilbert/no-growth line, the exact member of the HBL
triple is not pinned down in the audited sources. External consistency plus a
self-consistency theorem forces some premise of the applicable G2 package to
fail, but it does not identify which one.

### 4.3 Two selected diagonals without total substitution

Willard uses two selected fixed points here, and the comparison must keep them
separate.

**The fixed point used by G2.** Willard 2001, Theorem A.1 assumes that `alpha`
proves all of PA's `Pi^-_1` theorems. Its footnote 16 defines

`Theta(z) := forall x,y. (Subst(z,x) -> not Derive(x,y))`

and lets `N` be the code of `Theta`. It then states that `alpha` proves, for
`G := Theta(N)`,

`G <-> not Der(code(G))`.

This is exactly the selected `G <-> not □G` consumed by Theorem 1.1. Theorem
A.1 is `stated-only` in the Codification--the paper expressly omits a formal
proof--but footnote 16 displays this fixed-point step and says where its
`Pi^-_1`-strength hypothesis is used.

**The fixed point used by Group-3.** Write `B = UNION(A)` for the
non-self-referential Group-1 and Group-2 base, and let

`Subst_i -> N x N`

be the definable relation saying that the second code results from substituting
the first code into itself. The 1993 technical report, printed p. 37, states
that `IS(A)` is too weak to prove

`forall x. exists y. Subst_i(x,y)`,

but for every fixed numeral `k-bar` it can prove

`exists y. Subst_i(k-bar,y)`.

Categorically, the projection `Subst_i -> N` is not proved internally to be a
cover and has no proved global choice map, while the named fiber used in
Equations (A.2)-(A.3) is inhabited. Those equations define a different selected
fixed point `H = SelfRef(B,D)` satisfying

`H <-> not exists p. Prf_{B + H,D}(code(bottom),p)`.

This second fixed point forms the self-referential Group-3 axiom. Neither
selected construction is the universal fixed-point property of
van Dijk-Oldenziel Lemma 6.12, and neither proves total substitution. The
Theorem A.1 strength hypothesis scopes the first construction as used here; the
Group-3 construction and Proposition 1 have their own, different hypotheses.

### 4.4 The positive data that make an SJAS

Let `H := SelfRef(B,D)` and

`alpha* := B + H`.

Because `H` is constructed to express `Con(alpha*,D)` and is an axiom of
`alpha*`, that consistency proposition has a global proof in the syntactic
doctrine:

`self : 1 -> Con(alpha*,D)`.

That alone would be cheap: an inconsistent theory has such a point too.
Willard's Proposition 1 supplies the other half--under its stated hypotheses on
the base and tableaux apparatus, `alpha*` is externally consistent. The
Codification records its proof status as `full`.

For a Type-A `IS(A)` in the overlap of the stated hypotheses, the resulting
categorical profile is therefore

`selected G2 FP + selected Group-3 FP + cartesian contraction
 + Con point + external consistency + no uniform BoxMP`.

The HBL/Löb composite cannot turn `self` into a contradiction because its
evaluation leg is not an internal morphism. B-S's `S` is nondegenerate too; the
strictly stronger positive fact on Willard's side is that Willard supplies the
desired consistency point inside that nondegenerate theory.

For later Willard systems using `SelfCons_0` or `SelfCons_1`, replace `Con` by
the precisely coded Level-0 or Level-1 consistency predicate. One must not move
between those predicates without rechecking the corresponding proof relation
and consistency theorem.

---

## 5. The comparison square

The systems differ on two independent axes.

| | Structural diagonal on boxed hypotheses | Uniform internal HBL evaluation |
| --- | --- | --- |
| **AU** | present automatically (`x -> <x,x>`) | present by Proposition 6.8 |
| **B-S `S`** | **absent** (`□A -/-> □A tensor □A`) | present as L1 |
| **Willard Type-A `IS(A)`** | present in ordinary classical logic | **absent internally**; fixed proof-code instances only |

The fixed-point axis is separate again:

| | Strength of diagonalization |
| --- | --- |
| **AU** | every endomorphism `T : P1' -> P1'` (Lemma 6.12) |
| **B-S `S`** | every syntactically modalized fixed-point expression, including `G <-> not □G` |
| **Willard `IS(A)`** | selected `G <-> not □G` under Theorem A.1's strength hypothesis; a distinct selected Group-3 self-consistency fixed point; no internally proved total substitution |

Finally, self-justification adds positive and semantic data:

| | `1 -> Con` | external consistency |
| --- | --- | --- |
| **AU with full package** | ruled out when consistent | hypothesis of G2 |
| **B-S `S`** | **absent** by their §6 | **yes**, as forced by Proposition 4.1 and the consequence rules |
| **Willard `IS(A)`** | **present** as Group-3 | **proved** under Proposition 1's hypotheses (`full`) |

These tables should not be collapsed into a single “weakness” ordering. B-S
weaken structural logic while keeping the modal derivability rules. Willard
keeps structural logic and weakens what arithmetic can certify uniformly about
its proof predicate. The systems meet only at the consequence: one leg of the
G2 composite is missing. Both examples are externally nondegenerate; only
Willard also supplies the internal consistency point.

---

## 6. What has and has not been proved

**Proved in this comparison.** From the source definitions, the three
presentations instantiate the common interface as tabulated; the missing
morphism in each case is identified; and Theorem 1.1 explains why each missing
leg blocks the applicable HBL-G2 derivation. Proposition 4.1 together with the
B-S consequence rules entails external consistency of `S`. The B-S and Willard
failure points are not identified with one another, nor are Willard's G2 and
Group-3 fixed points.

**Cited rather than re-proved.** B-S cut admissibility, Proposition 4.1's
proof-omitted failure of formalized G2, and non-provability of `not □bottom`;
Willard's consistency-preservation
theorems, selected fixed-point constructions, and exact Type-A HBL (2) failure;
and the AU construction and its explicitly sketch-level §6 Löb derivation.
Their proof status and image anchors are recorded in
[`VERIFICATION.md`](VERIFICATION.md).

**Not claimed.**

- `IS(A)` is not shown to be an arithmetic universe or a sub-AU of `U_0`.
- The B-S monoidal category is not cartesian, and completing it cartesianly
  would add the very contraction whose absence is the theorem.
- The assignment `A |-> □A` need not be an endofunctor in Willard's internal
  doctrine; failure to transport arrows uniformly is the point.
- Unavailability of a required HBL or contraction instance blocks this G2
  derivation, but is never by itself sufficient for self-justification or a
  general proof that the conclusion is underivable.
- The G2 fixed point and Group-3 self-consistency fixed point are not the same
  proposition, and their source hypotheses are not interchangeable.
- Naming HBL (2) as a failed Type-A condition does not assert that every
  instance of `Nec` and `Four` holds for every possible choice of the inner
  basis `A`; no uniqueness-of-breach claim is needed here.
- The exact HBL breach is not asserted uniformly across every Willard
  deduction method.
- **Pakhomov's `H_{<ω}` is not in the comparison.** *Scope note added
  2026-09-04 during the Refinement's adversarial review.* R1–R3 establish it as
  one of only **two** demonstrated routes to a self-verifying theory — it proves
  `Con^pred(H_{<ω})`, and like Willard's systems it cannot prove successor
  totality — so its absence from §5's square is a gap, not a neutral choice.
  §0's closing contrast ("Willard establishes a consistent inhabitant of the
  self-consistency proposition") should be read as a statement about the three
  systems compared here, not about the field. What a fourth row would need is a
  categorical presentation of a *set* theory whose witness is a finite model on
  the superexponential cut; no Lawvere-side treatment of it is held, and this
  comparison does not attempt one. See `refined-sjas.md` §1 and
  `R3-the-margin.md` §5.1.

---

## 7. Primary-source anchors

- J. van Dijk and A. Gietelink Oldenziel, [*Gödel incompleteness through
  Arithmetic Universes after A. Joyal*](https://arxiv.org/abs/2004.10482),
  arXiv:2004.10482: Lemma 5.15;
  Theorem 5.20; §6 Definitions 6.1-6.3, Propositions 6.5-6.10, Lemma 6.12,
  Theorem 6.13, Remark 6.14. Local witness:
  [`AU source PDF`](../../lit/2004.10482.pdf).
- L. Beklemishev and D. Shamkanov, [*Some abstract versions of Gödel's second
  incompleteness theorem based on non-classical logics*](https://arxiv.org/abs/1602.05728),
  arXiv:1602.05728:
  Definitions 2.1, 2.3, 3.1-3.4, 3.7; Proposition 3.8; Remark 3.9;
  Propositions 4.1 and 4.3; Lemma 5.1; Theorem 5; §6. External consistency of
  `S` is the comparison's consequence of Definition 2.1, I4, Proposition 4.1
  and Theorem 5, rather than a quoted source theorem. Local witness:
  [`B-S source PDF`](lit/beklemishev-shamkanov2016_abstract_g2_nonclassical_arxiv_1602.05728.pdf).
- D. E. Willard, *Self-Verifying Axiom Systems and the Incompleteness Theorem*,
  TR 93-10 (the collated witness cannot predate April 1994): Proposition 1, printed
  p. 8 and §§4-6 (`full`); printed p. 12 (fixed-witness-only HBL composition);
  printed pp. 37-38 (fixed-instance substitution and Equations (A.2)-(A.3) for
  Group-3). Local witness:
  [`1993 technical report PDF`](../papers/1993technicalreport/willard1993_self_verifying_axiom_systems_tr93_10_searchable.pdf).
- D. E. Willard, [*Self-verifying axiom systems, the incompleteness theorem and
  related reflection principles*](https://doi.org/10.2307/2695030), JSL 66
  (2001): Theorem A.1 (`stated-only`) and its footnote 16 selected fixed-point
  justification, pp. 44-45; consistency results as indexed in the Codification.
  Local witness:
  [`2001 paper PDF`](../papers/willard2001_self_verifying_axiom_systems_author_jsl1.pdf).
- Corpus extraction and proof-status records:
  [`1993 extraction`](../codification/extraction/willard1993-tr.md),
  [`2001 extraction`](../codification/extraction/willard2001.md), and the
  [`result registry`](../codification/registry/results.md).
