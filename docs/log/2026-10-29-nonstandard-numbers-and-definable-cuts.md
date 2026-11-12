# Nonstandard numbers, definable cuts, and relative horizons

Date: 2026-10-29

Follows the [selector-proofs note](2026-10-29-selector-proofs-and-guarded-trust.md),
whose one-line comparison ("Artemov locates G2 in the internal quantifier's
reach to nonstandard x; Willard keeps the quantifier and bounds its reach in
use") this evaluation leans on.

This is the revised text, after an adversarial review the user requested the
same day. The [revision record](#revision-record) lists what changed.

## Requests (verbatim)

> Evalutate the following:
>
> Speculation: the Non-Standard Naturals are those not decidable as members of
> any computable definable cut (in Pudlak's sense).
>
> This would seem to accord with decidability as the mechanism which causes the
> reach of consistency statements ("no number n codes a proof p such that p
> proves a contradiction") to quantify beyond their grasp - the definition of
> the universal statement is too expansive, and cannot be made (by Tarskian
> undefinability perhaps) sufficient to restrict its domain to just that which
> its computational witness is sufficient to verify.
>
> The Non-Standard Naturals (NSN) can be called larger than any (computable)
> Nat because, for any given Natural, all Naturals less than it are Standard
> (by well-ordering, or an analogue, I expect), and so are some Naturals "not
> yet established" - the only ones not established are larger, and so the
> horizon beyond which Standardness is not assured can only be of greater
> magnitude.
>
> All of the evasions of G2 work like this, employing consistency as statement
> of a lower bound ("no refutation shorter than me"), such that the
> well-behaved prefix can be decidably searched for a lack-of-contradiction.
> Self-justification, at least in the Willard sense, goes yet further to ensure
> that the _means of performing this search_ cannot also construct any number
> beyond the searchable horizon, and so foreclose the possibility of ever
> constructing a "relatively non-standard" number that codes the proof of a
> refutation.
>
> If this line of thought is reasonable, then relativizing the designation of
> Standard vs Non-Standard on the basis of a theory's computational (i.e.,
> number constructing) capabilities clarifies, to me, what "internal" and
> "external" means wrt a theory.
>
> The most technically definitive version of this would be to cast a common
> frame on theories up to the strength of Q, such that, for weaker theories,
> external/Non-Standard Naturals are internal/Standard Naturals in a stronger
> theory.
>
> I have a vague sense that this is akin to what is done when one postulates a
> theory plus some cardinal, but am not certain of the details.

> Consider that you are wrong; adversarially review your response, and revise
> if/where warranted.

## Verdict

The core diagnosis is right. G2 lives in the reach of Con's quantifier to
elements the theory cannot verify. The region a theory can verify is governed
by the growth of the functions it can prove total.

Read literally, the proposed definition fails. Its natural repair, membership in
every definable cut, is not standardness. It is still a meaningful
theory-relative notion, and it draws exactly the line between the two kinds of
G2 evasion: it lets in no cut-free refutation, but it can let in a Hilbert one
(§5).

The direction claim holds in one frame and reverses in another. The common
frame requested exists only for sequential theories, and Q is not one.

## 1. The definition

- **Literal reading.** The whole domain, defined by x = x, is a definable cut.
  So no element lies outside every definable cut, and the set of nonstandard
  numbers would be empty.
- **Repair A: nonstandard means outside some definable cut.** This is sound,
  because every definable cut contains 0 and is closed under successor, so it
  contains every standard number. It is not complete:
  - In PA every definable cut is the whole model, by induction on the cut's
    formula.
  - By compactness a nonstandard element can lie in every definable cut. The
    theory T + {I(c) : I a T-cut} + {c > n̄ : n ∈ ω} is finitely satisfiable by
    a large enough numeral, since T ⊢ I(n̄) for each numeral.

  So the failure runs one way only: some nonstandard elements are misclassified
  as standard, and no standard element is ever misclassified.
- **Repair B: standard means provably in every cut, via a numeral.** This is
  true, since T ⊢ I(n̄), but it holds for any T and any cut. It amounts to
  "has a numeral", and the cuts do no work.
- **Decidability.** Prf_T is Δ₀. Every model therefore evaluates its own
  nonstandard proof codes exactly as it evaluates standard ones. Decidability is
  uniform across the horizon, so it cannot be what separates the two sides.

**Order.** Q proves x < n̄ → x = 0̄ ∨ … ∨ x = n−1 for each numeral, so the
standard part is an initial segment. The reason is this theorem scheme, not
well-ordering.

- In theories that prove x < S(x), such as IOpen and I∆0, no model with a
  nonstandard element is well-ordered, and the nonstandard part has no least
  element. There the horizon is a gap with no edge.
- In bare Q this can fail. ℕ ∪ {∞}, with S(∞) = ∞, ∞ + x = x + ∞ = ∞,
  ∞·0 = 0 · ∞ = 0, and otherwise ∞ · x = x · ∞ = ∞, is a model of Q. Its order
  type is ω + 1, and ∞ is the least nonstandard element.

At Q's level the horizon can have an edge, and the speculation's
"well-ordering, or an analogue" is not absurd there.

## 2. Undefinability, and where the domain can be restricted

- **ω is not definable once there is enough induction.** Where the theory has
  induction for the defining formula, a definition of ω would yield a least
  nonstandard element, whose predecessor would be standard.
- **In Q, ω can be definable in a given model.** In the ∞-model it is
  {x : S(x) ≠ x}. Still, no single formula defines ω in every model, by the
  compactness argument of §1.
- **Tarski is not the obstacle for Con.** It would be for uniform reflection:
  restricting attention to proofs of true sentences needs a truth predicate.

Restricting the quantifier:

- **To standard codes:** possible only as a scheme, {¬Prf(n̄, ⊥) : n ∈ ω}. This
  is Artemov's selector reading.
- **To the region the computational witness can verify:** possible with a
  single formula, a definable cut, for cut-free deduction (§3). So the
  speculation's "cannot be made sufficient to restrict its domain" is false for
  cut-free deduction and true for Hilbert deduction.

What is right: G2 says T + ¬Con(T) has models. Because T is actually
consistent, every refutation those models contain is nonstandard. The
unprovability of Con(T) is exactly the existence of these phantom witnesses.

## 3. "All evasions work by a lower bound"

- **A cut alone does not evade G2.** Pudlák 1985 (registry
  `Willard2006a#Thm1`): a consistent α ⊇ Q cannot prove even its cut-localized
  Hilbert consistency, for any definable cut.
- **A cut plus cut-free deduction does evade it.** Kreisel–Takeuti were first.
  Nelson showed Q proves a version about itself. Pudlák showed every finitely
  axiomatized sequential theory proves its own Herbrand or tableaux
  consistency localized to a definable cut (`Willard2002c#Rem6`,
  `Willard2009#CutDisambig`).

  The mechanism, as reconstructed here and not checked against Pudlák, is the
  subformula property. Every formula in a cut-free refutation of a finitely
  axiomatized theory is an instance of a subformula of its axioms. A partial
  satisfaction predicate of fixed complexity can therefore check soundness,
  and the cut guarantees that the terms' values exist.

  Hilbert proofs inside the same cut carry formulas of unbounded complexity.
  Converting them to cut-free form can cost a tower of exponentials, far
  outside the cut.
- **Willard is a different mechanism, and he says so.** "Our axiom systems do
  not have their consistency statements localized by an analog of" the
  cut-localized formula (`Willard2002c#Rem6`). Group 3 quantifies over the
  whole domain.

  The reading that the bound falls on use, not domain (a refutation can
  instantiate Group 3 only below its own size), is this project's own
  articulation. It comes from the 2026-10-28 and 2026-10-29 notes, and its
  arithmetic corollary is unchecked.
- **The "means of search" claim needs splitting by side.**
  - Hilbert-side systems (ISCE) do not prove successor total. That also
    disables Pudlák–Solovay cut shortening, which needs successor totality
    (`nachlass/refinement/R1-review.md`).
  - Tableaux-side systems prove addition, and hence successor, total. Their
    denial of multiplication's totality blocks Willard's tableaux form of G2
    (`Willard2002c#Rem4`), not cut shortening.

  In both cases nonstandard refutations are not foreclosed by the search.
  Group 3 is an axiom, so no model contains a D-refutation of any size. The
  weakness keeps that axiom consistent, a fact established externally and
  relative to the metatheory.

## 4. Where "relatively nonstandard" is exactly right

Take IS_D(A) with D = semantic tableaux, and suppose it proves successor total
together with the Π₁ facts about its Add and Mult relations that Solovay's
theorem needs (`Willard2006a#Thm2`).

1. By Solovay's theorem it cannot prove its Hilbert consistency.
2. So some model M contains a Hilbert refutation p of the system, and p is
   nonstandard.
3. M satisfies Group 3, so M contains no tableau refutation. The cut-free
   conversion of p must therefore break inside M.
4. The natural place for it to break is size: the conversion needs a tower
   over p, and M need not contain one.

So p is a refutation under one deduction method with no counterpart under the
other, because the conversion outgrows the model. What is relative is closure
under growth, not standardness. The argument is mine. Step 4 is a diagnosis,
not a proved localization of the failure.

## 5. Relativizing: the T-standard part

Standardness is absolute once the metatheory is fixed. Relativizing it means
changing the metatheory or adding a standardness predicate, which is what
Hrbacek's relative set theory, Péraire's RIST and Nelson's IST do.
Relativizing to the object theory's growth, with the metatheory held fixed,
gives a different notion:

> **Definition.** For M ⊨ T, an element a is *T-standard* iff a ∈ I^M for every
> T-definable cut I.

Properties:

- The T-standard part contains ω.
- It can properly contain ω, by compactness (§1).
- In PA it is the whole model.
- It lies inside every accessibility cut I_f = {x : f(x) exists} that T proves
  to be a cut.

> **Claim (mine, from the cited theorems; not machine-checked).** Let T be
> consistent, finitely axiomatized, sequential and ⊇ Q. Then:
>
> 1. in every model of T, no T-standard element codes a cut-free (Herbrand or
>    tableaux) refutation of T;
> 2. some model of T has a T-standard element coding a Hilbert refutation of T.
>
> *Proof.*
>
> 1. Pudlák gives a T-cut I with T ⊢ ∀x (I(x) → ¬CFPrf_T(x, ⊥)), and the
>    T-standard part lies inside I.
> 2. A finite intersection J of T-cuts is a T-cut. By Pudlák's G2 for cuts,
>    T + J(c) + Prf_T(c, ⊥) is consistent. So by compactness,
>    T + {I(c) : I a T-cut} + Prf_T(c, ⊥) is consistent. ∎

The speculation's "well-behaved prefix", made precise as the T-standard part,
is exactly where the cut-free evasion works and the Hilbert evasion fails.

**Direction.** It depends on the frame.

- **Model frame (the speculation's direction).** In models of T + "f is total",
  the cut I_f is the whole model, so the f-horizon disappears. An element
  beyond I_f in a model of T falls inside I_f in any end extension satisfying
  "f is total". Whether such end extensions exist is delicate for I∆0.
- **Interpretive frame (the reverse).** Q interprets S¹₂, which is mutually
  interpretable with I∆0 + Ω1, on a cut. The stronger theory's universe is an
  initial segment of the weaker theory's model, and strength is bought by
  shortening, which buys induction and closure.

A common frame needs the interpretive one, since it compares theories inside a
single structure.

## 6. The common frame

For sequential theories it exists. Pudlák's comparison of interpretations
says, as recalled here and not checked, that a definable cut of one number
interpretation is definably isomorphic to an initial segment of another.

Q is not sequential (Visser; Jeřábek shows PA⁻ is, "in contrast to" Q). Q
reaches the frame only through S¹₂ on a cut, and interpretation on a cut does
not preserve sequentiality. So the frame breaks at Q itself, not only below it.

Willard's systems are not "below Q" in a linear order. They lack Q's totality
axioms, but they prove all of A's Π₁ theorems, so they are incomparable with Q.
The sharpened question: is there a cut-comparison theorem for non-sequential
theories that lack successor or multiplication totality? A first test: does
IS_D(A) interpret Q, or S¹₂ on a cut? This note does not know.

## 7. The large-cardinal analogy

The common ground is closure points. Below an inaccessible κ the universe is
closed under the set operations, and V_κ is a model on an initial segment. In
arithmetic, a cut closed under f plays the same role.

The directions differ:

- V_κ carries a weaker theory (ZFC) inside a stronger one.
- Q's shortened cut carries a stronger theory (S¹₂) inside a weaker one.

In arithmetic, shortening buys induction. In set theory the ambient universe
already has the closure, and the initial segment becomes a set model.

As a consistency-strength analogue, totality of faster functions plays the
large-cardinal role. For example, cut elimination becomes provable once
superexponentiation is total (Hájek–Pudlák).

Literature already using this vocabulary (pointers from memory, unchecked):

- **Levels of standardness:** Hrbacek's relative set theory and Péraire's RIST.
- **"Horizon" as a technical term:** Vopěnka's Alternative Set Theory.
- **The predicative and feasibility line:** Nelson, *Predicative Arithmetic*
  (1986); Parikh, "Existence and feasibility in arithmetic" (1971).

## 8. Follow-up: can decidability ground the distinction?

> The point of this speculation is to determine whether "standard" vs
> "non-standard" naturals can be distinguished on the basis of decidability, a
> reduction of the model theoretic distinction to a computability result.

**Answer.** Not to decidability. From outside, standardness is semi-decidable;
from inside, it is not even definable. The one genuine computability
characterization works on whole models, not on elements, and it fails at Q.

- **Elements.** An element a is standard iff counting down from a reaches 0
  after finitely many steps, i.e. iff that countdown halts.
  - This restates the definition: "halts" means "in standard-finitely many
    steps".
  - Inside M, every countdown halts, after a internal steps. So "standard"
    means internal and external halting agree. "Nonstandard" means the
    countdown halts inside the model but not outside it.
  - Given a presentation of M with computable successor, the standard part is
    c.e.: enumerate 0, S0, SS0, …. That is semi-decidability, not
    decidability.
- **Models (Tennenbaum).** Up to isomorphism, ℕ is the only computable model
  of PA.
  - This holds down to IE₁ (Wilmers, via MRDP). It fails for IOpen
    (Shepherdson 1964) and for Q, whose ℕ ∪ {∞} model is computable.
  - The proof needs a nonstandard element coding a separator of a
    recursively inseparable pair. Getting one needs a Π₂ coding theorem plus
    overspill.
  - IS_D(A) has neither, so I expect it to have computable nonstandard models.
    This is unchecked.
- **Sentences.** A model has a phantom witness to a Π₁ sentence iff some
  standard machine halts in it but not in ℕ. That is Σ₁-unsoundness, not
  nonstandardness: nonstandard models of Th(ℕ) have no phantom halting.
  - G2 turns on this narrower property. Group 3 forbids phantom halting of the
    refutation search.
- **Circularity.** "Computable" is defined over standard-finite runs.
  Halbach & Horsten (2005) use Tennenbaum to single out the intended model.
  Button & Smith (2012) reply that this cannot explain our grasp of ℕ, since
  grasping "computable" needs the same thing.
  - So the reduction succeeds as classification, above IE₁.
  - It fails as grounding.

## 9. Follow-up: halting inside without halting outside

> halting inside without halting outside. <- Elaborate.

**Definition.** Write H(e) := ∃t Run(e, t), where the Δ₀ predicate Run(e, t)
says that t codes a halting run of machine e.

- e halts *outside* iff ℕ ⊨ H(e).
- e halts *inside* M iff M ⊨ H(e).
- e *phantom-halts* in M iff it halts inside M but not outside. Its witness t
  is then necessarily nonstandard.

**One direction never fails.** A real halting run is a standard number. By
Σ₁-completeness, Q proves Run(ē, t̄), so every model contains that run.

**The other direction can fail.** Run is Δ₀, so it is evaluated identically on
standard and nonstandard arguments. The existential can therefore be witnessed
by a nonstandard t that the checker accepts.

**Shape of a phantom run.**

- At standard steps it agrees with the real run, by induction in the
  metatheory.
- In a countable model of PA, [0, t] has order type ω + ℤ·η + ω*.
- Every adjacent pair of configurations passes the transition check.
- Stepping forward from the start never reaches t, and stepping backward from
  t never reaches the start.

A checker can verify each transition locally, but it cannot verify that the
run is finite, because finiteness is a global property of the run. That is why
decidability cannot separate standard from nonstandard elements.

**What a theory controls.**

- By completeness, T ⊢ ¬H(e) iff no model of T contains a halting run of e.
- Define Gap(T) = {e : e diverges, and T ⊬ ¬H(e)}. For T ⊇ Q, each member
  phantom-halts in some model of T.
- **G1.** Gap(T) ≠ ∅ for consistent c.e. T, because true non-halting is not
  c.e.
  - One witness is the diagonal machine d: "halt on finding a T-proof that d
    never halts".
- **G2.** The refutation search r_T lies in Gap(T).
- **Nonstandard is not the same as phantom.** Models of Th(ℕ) are nonstandard
  but have no phantom halting.
- **Artemov's scheme** {¬Prf(n̄, ⊥)} excludes halting at every standard time.
  Phantom halting happens at no standard time.

**Where growth enters.** G2's proof turns a phantom halt of d at t into a
phantom refutation, in three steps:

1. d halted, so it found an internal proof σ of ¬H(d).
2. Formalized Σ₁-completeness (D3) converts d's run of length t into an
   internal proof π of H(d). The size f(t) of π is fixed by the deduction
   method, term language and encoding.
3. σ and π together form an internal refutation, so r_T phantom-halts too.

This works only if f(t) exists in M.

**Self-justification in these terms.**

- Group 3 is the axiom ¬H(r_D): it removes r_D from the gap by fiat.
- Willard's systems do not prove f total. In models where f(t) is missing, a
  phantom halt of d at t stays inert: it never becomes a refutation.
- The horizon of §5 is I_f = {t : f(t) exists}.
- The gap stays nonempty, because provable non-halting is c.e. and true
  non-halting is not.
- This reading follows the project's guarded-trust notes. It is checked at toy
  level only; the arithmetic version is unchecked.

**Provenance** (added 2026-10-30, at the user's request).

- **Terminology is mine.** "Phantom halt" and Gap(T) were coined in this
  conversation. The nearest earlier project usage is R4's "phantom
  certificates" (`nachlass/refinement/R4-certificate-calculus.md`, D2 row).
- **Components are standard:**
  - Kleene's T-predicate and Σ₁-completeness.
  - Δ₀-absoluteness over the standard initial segment.
  - The completeness theorem.
  - G1 via the halting problem, and Kleene's recursion theorem.
  - The derivability conditions (Hilbert–Bernays, Löb).
  - The order type ω + ℤ·η (Kaye 1991; Kossak–Schmerl 2006).
- **Nearest literature on computation of nonstandard length:** Woodin's
  universal algorithm (Blanck–Enayat; Hamkins,
  [arXiv:1801.04599](https://arxiv.org/abs/1801.04599)).
- **The growth step and the "inert beyond I_f" reading** are my synthesis.
  They draw on Willard's doubling/squaring explanations (`Willard2002c#Rem4`)
  and the project's guarded-trust notes.

## Status

- **§§1–2:** standard facts, with the compactness, ∞-model and literal-cut
  arguments given in full.
- **§3:** rests on the registry rows cited (Willard's citations of Pudlák,
  Nelson, Kreisel–Takeuti and Solovay). The primary sources have not been
  re-read. The subformula-property mechanism is a reconstruction.
- **§§4–5:** the arguments and the T-standard claim are mine, derived from the
  cited theorems.
- **§6:** "Q is not sequential" and "S¹₂ is interpretable on a cut in Q" were
  checked against the abstract and introduction of Jeřábek,
  [arXiv:1201.5835](https://arxiv.org/abs/1201.5835). The exact statement of
  Pudlák's comparison was not checked. The IS_D(A)/Q question is open here.
- **§7:** the pointers are unchecked.

## Revision record

Adversarial review, same day. Corrections to the first version:

1. The definition was said to fail "in both directions". It fails in one: the
   repair "outside some cut" is a sound sufficient condition. The first version
   also missed the literal problem, that the whole domain is a definable cut.
2. Nonstandard models were said not to be well-ordered, with no least
   nonstandard element. That is false for Q (the ∞-model has order type ω + 1)
   and true only once x < S(x) is provable.
3. The domain restriction was said to exist only as a scheme. That contradicted
   §3: for cut-free deduction a single cut formula does it.
4. The cut-free evasion was attributed to incompressibility. The positive
   mechanism is the subformula property. Incompressibility only explains why
   the Hilbert case does not reduce to it.
5. Denying "successor or multiplication" was said to disable cut shortening.
   Only the successor denial does.
6. §4 dropped the Π₁ hypotheses of Solovay's theorem, and it used "encoding"
   for the deduction method, conflating two claim coordinates.
7. "Relativize horizons, not standardness" and "the direction is the reverse"
   were overstated. Standardness relativizes with the metatheory; the direction
   depends on the frame; and the T-standard notion rescues the speculation in
   precise form (§5, new).
8. The common frame was placed "at Q and above". Q is not sequential, and
   Willard's systems are incomparable with Q rather than below it.
9. The guarded-use reading was presented as established. It is this project's
   unchecked articulation.
10. The large-cardinal analogy missed that the arithmetic and set-theoretic
    directions differ.
