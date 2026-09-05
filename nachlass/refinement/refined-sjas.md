# The Refined SJAS

*The essential idea of Willard's self-justifying axiom systems, identified and
explicated — departing from his presentation where the idea is clearer without
it.*

> **Relation to the Codification.** [`../codification/codified-sjas.md`](../codification/codified-sjas.md)
> states what Willard proved, in his vocabulary, with his provenance. This
> document asks what he *found*. It may reformulate freely; where it does, it
> says so.
>
> **What each kind of claim rests on.** Claims about **Willard's corpus** rest on
> the codification's registries and on page images recorded in
> [`VERIFICATION.md`](VERIFICATION.md). Claims about the **secondary literature**
> (Pakhomov, Beklemishev–Shamkanov, van Dijk–Oldenziel, Brown–Palsberg) rest on
> page images of those papers, which the Codification never covered. Claims about
> **Pudlák** rest on neither: his paper is not held, and what this document says
> his proof requires is what Willard and Pakhomov report it requires — gap
> **G39**, §9.
>
> *Drafting history is not kept in these documents.* Every correction made to
> this stage is tabulated in [`VERIFICATION.md`](VERIFICATION.md); the text below
> states what the Refinement now holds.

---

## 1. The claim

Thirty years of this literature can be compressed to one sentence.

> **A system can consistently assert its own consistency when, for every proof
> it can write, it can construct within its own means a bounded semantic witness
> that the proof is not a refutation.**

**What that sentence's status is, since this document asks the same of every
claim it makes about Willard.** It is *this Refinement's identification*, not a
theorem of the corpus. It is supported by two instances — Willard's bounded
valuation on a tableaux branch and Pakhomov's bounded finite model on the
superexponential cut — and generalised from them, not derived. Its sharp
arithmetic form below is Willard's own Definition 4.5, and **the corpus proves
that form sufficient only by a `sketch`** (`Willard2011` Lemma 4.6;
`R3-the-margin.md` §§2.2, 6). Read the sentence as the best available statement
of what the literature has found, not as something established.

In Willard's arithmetic setting the condition becomes a comparison of two sizes:

> **a refutation must cost more than the system's own reach affords.**
>
> Sharply (§5, and `R3-the-margin.md`): `Log(q_β) ≥ ♯(β) + 2` for every
> inconsistent `β` above the base — the least Gödel number of a proof of `0=1`
> exceeds, by at least two bits, the envelope within which `β`'s sentences stay
> true.

**A correction to how this section used to put it.** Earlier versions gave the
arithmetic form as *"naming an integer must not be cheaper than proving
something about it"* and said the rest of the document develops that. It does
not, and §§5–5.1 are why. The naming slogan is the intuition the programme
started from, but of the corpus's five boundary dials the three now *proved* to
move the margin — `ℜ`, `Herb−k`, `Z` — are **apparatus** dials that have nothing
to do with naming: they change which intermediate theorems a proof may cite. The
one genuinely about naming, Hybrid(H), is precisely the dial whose term this
Refinement could **not** identify. So naming is one instance of the form, and
currently the least supported one.

Everything else in the corpus — the four axiom groups, the five deductive
apparatuses, the **ten** formula-class notations, the five boundary dials — is
machinery for making that comparison precise in one setting or another, or for
locating the point at which it stops holding.

**The arithmetic form is not the whole idea.** Pakhomov's `H_{<ω}` (2019)
satisfies the general condition with a different witness. The precise statement
(p. 4, image-verified) is that inside **EA** one constructs, for a Gödel number
`p` of a proof, a finite model `M` **of size `≤ 2⁰_p`** satisfying exactly the
axioms *occurring in `p`* — where `2⁰_p` is **superexponentiation**, `2^y_0 = y`
and `2^y_{x+1} = 2^{2^y_x}`, and the whole argument is relativised to the
**superexponential cut** `S = { x : 2⁰_x is defined }`.

Two qualifications. The bound is *superexponential*, not small — "bounded"
here means bounded by a function EA can handle **on the cut**, and the cut is
where the restriction lives. And the consistency proved is `Con^pred`, a
**predicate-only `Π^pred₁`** consistency sentence, with the model construction
carried out for the higher-order `H^ω_{<ω}` and transferred.

Willard bounds a *partial valuation on a tableau branch*; Pakhomov bounds a
*full finite model of the axioms actually used*. Both bounds are witnesses; only
their shape differs.

**`H_{<ω}` is not a counterexample to the growth restriction.** It does prove
every individual hereditarily finite set exists — which is why it can look
like one — but Pakhomov states in the Introduction, **"Important restriction
here is that both `H` and `H_{<ω}` could not prove totality of successor
function"**, a sentence straddling the page break at **pp. 3–4** — and again,
for `H` alone, at **p. 22**. So it is growth-restricted, by the *same* route
as Willard's Hilbert line — and it must be, since Pudlák's cut-shortening
needs successor totality and that is what both are evading. Pakhomov positions
his system not as a different mechanism but as **"a more natural example of
this kind"**, his objection to Willard's being that "some of [the] axioms are
constructed using Diagonal Lemma".

The generalisation above still stands, and the corpus itself shows why it
should: `Willard2006a` Theorem 6 (**`sketch`**) gives **`ISINF(A)`**:
*Infinitely Far-Reaching* — a finite subset proves `∃x Pred^N(x) = 1` for
every `N` — while, in Willard's words, "Infinitely Far-Reaching **without
sustaining an ability to prove successor is a total function**". That is
structurally what `H_{<ω}` is. Two witnesses of the same shape, one arithmetic
and one set-theoretic, is a reason to state the criterion in terms of the
witness rather than the naming rate. It is **not** a falsification, and
[`R1-review.md`](R1-review.md) §2 is corrected to say so.

The rest of this document argues that this is the right sentence: §§2–4 say
why the usual reading of Gödel's Second Theorem misidentifies the obstacle, §5
gives the criterion its sharp form, §6 discharges the Rosser hook, §7 says
what the idea costs, §8 shows the same phenomenon outside arithmetic, and §9
says what is not settled.

---

## 2. Where the breach goes

The standard reading of G2 is that self-reference is the problem: a system
strong enough to talk about itself can build "I am not provable", and that
sentence destroys any attempt at self-certification.

That reading is wrong, and Willard's systems are the counterexample. **They
contain a self-referential fixed point as an axiom and are consistent.**

**Which fixed point, precisely** — a distinction this section elided until
2026-09-04, and which [`lawvere-sjas.md`](lawvere-sjas.md) §1.3 (R6) states
sharply. Two *selected* diagonals occur in this subject and neither implies the
other:

| | Sentence | Role |
| --- | --- | --- |
| **the G2 diagonal** | `G ↔ ¬□G` | the ingredient the limitative argument consumes |
| **the self-consistency diagonal** | `H ↔ Con(B + H, D)` — the proof predicate depends on the theory *including `H`* | how a self-referential consistency assertion is formed; adjoining it is what makes a system self-justifying |

**Willard's systems take the second as an axiom, not the first.** Group-3 is
`H`, not `G`. Everything below about "the fixed point being free" is about the
availability of *selected* diagonals in general; it is not a claim that Willard's
systems contain the Gödel sentence.

### 2.1 Selected diagonals are free; the derivability conditions are not

Lawvere's Fixed Point Theorem makes the point structurally. In a cartesian
closed category, if there is a weakly point-surjective `f : A × A → B`, then
**every** endomorphism `t : B → B` has a fixed point. Cantor, Russell, Tarski,
Turing and Gödel's *First* theorem are the contrapositive: exhibit a
fixed-point-free `t`, conclude no such `f` exists. (Yanofsky develops the family;
the held witness is `../../lit/(Elements in Applied Category Theory) Noson S.
Yanofsky - Theoretical Computer Science for the Working Category
Theorist-Cambridge University Press (2022).pdf`.)

What Lawvere gives is **existence**, and it costs almost no theory. Kleene's
recursion theorem is the arithmetic form, and every affirmative system in the
corpus builds its Group-3 axiom by a fixed-point construction (codified §5.1).

**But the construction is not internally available in general**, and R6 §4.1's
distinction between what a metatheorist may do with proof codes and what the
weak theory itself proves is what separates the two. `IS(A)` **cannot** prove
`∀x ∃y SUBST_i(x,y)`; `Willard1993-TR` printed p. 37 says so outright ("Since
it does not recognize multiplications as a function, `IS(A)` will clearly be
too weak to prove that…"), adding that it *can* prove `∃y SUBST_i(k̄,y)` "for
**any fixed integer `k̄`**", and that "the latter will be sufficient … to
formally define the particular instance of the reflection principle needed to
define `H`". So the recursion theorem in its general form is precisely what is
*not* internally available. What is used is a **selected instance at a fixed
numeral** — which is why §2's table calls both diagonals *selected*.
Willard2001's own footnote makes the division explicit — the fixed-point
identity "is the **only** aspect of the proof of the Hilbert-Bernays Theorem
that needs Peano Arithmetic for justifying it", and "the remainder of the
proof rests solely on" the derivability conditions.

So the paradox is not in the diagonal. It is in what must hold *around* it.

### 2.2 Willard's own frame: the breach is obligatory, its location is not

`Willard1993-TR` states the governing fact four times — the fourth, on printed
p. 12, is §2.3's — and it is the frame the whole corpus is built in:

> "Gödel's Incompleteness Theorem requires that **every self verifying system
> must breach in some way one of the three fundamental Hilbert-Bernays
> conditions**. **The breach, which can be very subtle, may arise because of
> either the axiom system employed or the choice of deduction method, but not
> necessarily due to both.**"

`Willard2001` Appendix A sharpens the theorem being breached so that it applies
to systems far below PA — **Theorem A.1**, whose status must be carried here
because everything in this section rests on it: it is **`stated-only`**. Willard
writes "we will not give a formal proof of Theorem A.1 in this very short
appendix", and argues instead, in footnote 16, that its proof "is fully
identical to Hilbert and Bernays' well-known prior proof construction". That is a
reduction, not a proof, and the frame this document builds on it inherits the
status. If `α` proves all of PA's `Π⁻₁` theorems and `Der` satisfies

> **(1)** `α ⊢ Φ ⟹ α ⊢ Der(⌜Φ⌝)` · **(2)** `α ⊢ {Der(⌜Φ⌝) ∧ Der(⌜Φ ⊃ Ψ⌝)} ⊃ Der(⌜Ψ⌝)` · **(3)** `α ⊢ Der(⌜Φ⌝) ⊃ Der(⌜Der(⌜Φ⌝)⌝)`

then consistent `α` cannot prove `¬Der(⌜0=1⌝)`. Willard notes this "differs from
the classic Hilbert-Bernays theorem only by not requiring `α` to be an extension
of Peano Arithmetic", and that proving PA's `Π⁻₁` theorems "is actually the core
fact that is needed by the classic proof". `Willard2011` puts the consequence
plainly: "**Conventional** generic configurations `ξ` will satisfy the
Hilbert-Bernays derivability conditions… Their `G^ξ_k(θ)` will thus be
**automatically inconsistent**."

Willard's systems prove all of PA's `Π⁻₁` theorems. They therefore *must* breach
one of (1)–(3). **The research programme is the study of where to put the
breach**, and that is why its results trade design parameters against one
another rather than converging on a single system.

Willard contrasts his choice with Feferman's: `[Fe60]` "illustrates a
self-verifying system which employs **all the axioms of Peano Arithmetic**, but
which draws upon a **deduction method** which captures the numerical but not
intensional definition of classic deduction", whereas `IS(A)` keeps "a deduction
method as natural as semantic tableaux" and instead weakens "several of the
axioms of Peano Arithmetic, **including the axiom that the multiplication
function is total**". Same obligatory breach; opposite placement.

### 2.3 Willard names the condition, and names it as a uniformity failure

**For the tableaux line this is not an inference. Willard states it.**
`Willard1993-TR`, printed p. 12 — the fourth of the document's four passages on
the Hilbert–Bernays conditions, and the one that answers the question the other
three raise:

> "A well-known corollary to the Gödel Incompleteness Theorem is that every
> self-verifying system must violate one of the Hilbert-Bernays conditions
> [HB39, Me87]. **For the case of `IS(A)`, the difficulty is that only in the
> *degenerate case* where `x` and `y` are *fixed constants* can `IS(A)` prove
> the Hilbert-Bernays condition that if `x` is a proof of `α` and `y` is a proof
> of `α ⊃ β` then there exists some `z` that proves `β`.**"

That is **condition (2)**, named for `IS(A)` — the Type-A, semantic-tableaux
system. **The two formulations line up, and it is worth saying how**, because
§2.3a's argument turns on where the quantifiers sit. Theorem A.1's (2) is
schematic in the *sentences* `Φ`, `Ψ` and hides the proof quantifier inside
`Der(x) = ∃y Derive(x,y)`; the TR's phrasing unpacks that same content over
*proof codes* — "if `x` is a proof of `α` and `y` is a proof of `α ⊃ β` then
there exists some `z` that proves `β`". Same condition, quantifiers written out.
That is why "only for fixed constants `x` and `y`" is a failure of (2) and not
of some weaker schematic variant. The Codification records the same reading:
`../codification/extraction/willard1993-tr.md` §3.7c is titled *"Which
derivability condition fails"*.

**And the *shape* of the failure is not what one would guess.** Condition (2)
does not simply collapse. It **holds for fixed constants and fails uniformly**
— `IS(A)` can prove the composition principle for any particular `x`, `y`, and
cannot prove it for variables. That is a *uniformity* failure, and it is not
the same as the cut-elimination length blowup one might reach for instead
(`Willard2002a` Thm 2.2 — `sketch`, and headed "Proof Sketch" on the page —
which says the combined proof exists but may be super-exponentially longer).
The two are related — a uniform statement would have to bound the blowup for
all `x`, `y` at once — but Willard's own reason is the quantifier, not the
length.

This matters because the shape is the corpus's signature. The extraction record
flags the same pattern at `SUBST_i(k̄,y)` for fixed `k̄`, at Lemma 5.3's
`T(p,k)` for fixed `k` or fixed `p`, and at `Scalar_k` — **fixed-parameter
totality versus uniform totality is the recurring device of the whole programme**
(obligation **O22**). Condition (2)'s failure is one more instance of it, not a
separate phenomenon.

### 2.3a The Hilbert line — still an inference, and a weaker one

No comparable statement for `ISREF`, `ISCE` or `IQFS` has been located. What
follows is therefore the Refinement's own, and is offered with less confidence
than §2.3.

The natural guess is condition **(1)**: asserting `Der(⌜Φ⌝)` means asserting a
proof exists, which means naming it, and with no totality axiom the naming
convention is the only route to large integers. **But that guess is probably
wrong**, for a reason the tableaux case supplies. Condition (1) is a *rule*,
applied one `Φ` at a time, with no uniformity demand — and the Hilbert-line
systems are built to satisfy exactly that: `ISCE(A)`'s **Continuous Expansion**
property means the naming axioms with Gödel numbers below `K_i` already prove
the existence of an integer above `K_{i+1}`, so for any *particular* proof `p`
the system can name `p`. What these systems lack is the *uniform* `∀x ∃y`
totality statement — which is the shape that bites on **(2)** and **(3)**, not
on (1).

So the honest position is: the tableaux line breaches (2), on Willard's own
authority; the Hilbert line breaches (2) or (3), by analogy with the corpus's
uniformity pattern, and the earlier assignment to (1) is withdrawn.

### 2.3b Why the "controls" are not evidence

Two results look as though they confirm the condition-(2) assignment: restoring
cut via `Xtab` makes `IS_Xtab(β)` inconsistent (`Willard2020` Thm 4.5), and the
multiplicative naming convention is fatal (`Willard2006a` Thm 4).

**They confirm nothing.** Both have the form *P ⟹ Q; Q; therefore P*. Neither
rules out any other assignment, and this document uses both elsewhere for
*different* explanations: `R3-the-margin.md` §3.2 cites the same Thm 4.5 to show
the `Z` dial lowers refutation cost, and **§5's table below** treats the
multiplicative convention's failure as evidence about the **naming instance** of
the criterion. A fact compatible with three explanations is evidence for none of
them.

Both also carry hypotheses that must travel with them. `Willard2020` Theorem 4.5
is **`sketch`**, and holds only "**whenever `β` proves some conventional `Π*₁`
theorems stating that addition and multiplication satisfy their usual
associative, commutative, distributive and identity properties**".
`Willard2006a` Theorem 4 requires three: the multiplicative axioms, all of PA's
`Π⁻₁` theorems, **and** the Concise Encoding property. Its **Theorem 4\***
(Remark 1) is stronger on both counts — it "will drop Theorem 4's 'Concise
Encoding' assumption and *also isolate a `Π⁻₁` theorem `W`* of Peano Arithmetic,
where no consistent r.e. axiom system `α ⊃ W`, using the multiplicative naming
convention, can formally verify its own Hilbert consistency" — at the cost of an
abbreviated Appendix C proof. So the control is available with two hypotheses,
though the logical objection above is unaffected either way.

### 2.3c A correction that survives

**It is not true that Willard's systems deny successor totality.** Only the
Hilbert line does. The tableaux line proves successor *and* addition total and
gives up multiplication alone. Which totality is surrendered is one of the design
parameters, not a fixed feature of the programme.

One qualification on "cut-free". The tableaux line — `IS(A)`, `IS_D(A)` and
their kin, which codified **§3.1** tabulates as the Type-A row, "successor,
addition … the whole tableaux line" — includes `IS_D(A)`, whose `D` ranges
over **`Tab-U*₁-List`** — restricted modus ponens on `Π*₁`/`Σ*₁` — and that is
the case `Willard2005` Theorem 5 actually proves. So the line is not uniformly
cut-free; what varies across it is *the class on which composition is
available*, which is the `ℜ` dial. Calling the whole line "cut-free" erases
the distinction **§5.1** is simultaneously trying to draw.

### 2.4 What each condition licenses, and what contraction licenses

The three conditions are not three versions of one thing. Taken one at a time:

| | Licenses | |
| --- | --- | --- |
| **(1)** | **internalisation**: from *having* a proof, assert that one exists | once |
| **(2)** | **composition**: combine proofs of `Φ` and `Φ ⊃ Ψ` into a proof of `Ψ` | two *different* proofs |
| **(3)** | **iteration**: from the assertion `Der(⌜Φ⌝)`, assert `Der(⌜Der(⌜Φ⌝)⌝)` | **the same proof, one level up** |

**Reuse is (3).** It is the condition that takes a statement already *about* a
proof and makes that statement itself a proved object — the proof is used again,
reflected upward. (1) internalises once; (2) composes two distinct derivations.

**And failing (1) does not entail failing (3).** One might argue that it
should, since proving `Der(⌜Der(⌜Φ⌝)⌝)` internalises (1) — but the corpus is
exactly where that fails. (3) is an *implication*, `Der(⌜Φ⌝) ⊃
Der(⌜Der(⌜Φ⌝)⌝)`, and an implication holds vacuously when its antecedent is
refuted. Any `α` proving `¬Der(⌜Φ⌝)` satisfies that instance of (3) while
failing (1) — and Willard's systems prove `¬Der(⌜0=1⌝)` **by construction**.
The vacuous case is not exotic here; it is the point of the programme.

### 2.5 Contraction is a structural rule, not one of the three

It is tempting to say that Beklemishev–Shamkanov's `□`-contraction is one of the
derivability conditions restated. It is not, and their proof says where it is
used.

Their Definition 3.7 gives **plain** contraction, `Γ, φ, φ ⊢ ψ ⟹ Γ, φ ⊢ ψ` — a
rule of the ambient consequence relation about reusing a **hypothesis**, not
about reusing a proof. (The `□`-restricted form, `Γ, □φ, □φ ⊢ ψ ⟹ Γ, □φ ⊢ ψ`,
is the *weakening* of the requirement offered in their Remark 3.9; Proposition
3.8 and Theorems 3–4 assume the plain rule. Running the two together is the
conflation §3 of
[`R2-beklemishev-shamkanov-assessment.md`](R2-beklemishev-shamkanov-assessment.md)
warns this repository's affine-tree line against.)

In Proposition 3.8 it is consumed at one specific step, establishing their
condition **C3** — quoted from the **page image**, because the text layer strips
every `□`:

> "By Lemma 3.3(i) we have: `φ, ¬φ, ⊤ ⊢ ⊥`. Hence, `φ, ¬φ ⊢ ⊤ → ⊥`, therefore
> **`□φ, □¬φ ⊢ □¬⊤`** by Condition 1. **The rules of transitivity and contraction
> imply that, if `Γ ⊢ □φ` and `Γ ⊢ □¬φ`, then `Γ ⊢ □¬⊤`.**"

So the step is not from `φ` and `¬φ` to absurdity. It is from **`□φ` and `□¬φ`**
— two *boxed* derivations off the same context `Γ` — to **`□¬⊤`**, which under
their abbreviation `⊠φ := □(φ → ⊥)` is `⊠⊤`, the **formalized** inconsistency
assertion. What contraction licenses is merging the two uses of `Γ`. Without it,
a context that proves `□φ` and `□¬φ` does not thereby prove `⊠⊤`.

That is more elementary than any of Löb's conditions and sits underneath all of
them — but it is a statement about the *formalized* assertion, not about
absurdity itself, and §2.5a records what follows from that.

### 2.5a Their system is not self-verifying, and they say so

Beklemishev–Shamkanov's contraction-free `K4` is **not a route to
self-verification**, and the paper says so in its concluding section
(§6, image-verified):

> "We remark that the system `S` does **not** provide a counterexample to the
> **non-formalized** version of G2, since `⇒ ¬□⊥` is not provable."

`S` fails to prove its own consistency. It refutes *formalized* G2 only —
Gödelian fixed points exist and `⊠⊤` is not derivable from them — which is a
result about the argument, not about a self-verifying theory. The authors are
explicit that no such theory is in hand:

> "For one such system, considered by the second author of this paper, the rule
> of `□`-contraction is admissible, which according to our results still yields
> G2. Thus, **we are still missing convincing examples of mathematical theories
> based on weak logics for which G2 would fail**."

That second sentence also settles a tempting hope: going contraction-free at
the object level does not suffice, because `□`-contraction can remain
admissible. It is the same caution
[`R2-beklemishev-shamkanov-assessment.md`](R2-beklemishev-shamkanov-assessment.md)
§3 records for this repository's affine-tree line, and it applies to this
classification too, not only to a neighbouring project.

### 2.5b What the picture actually is

Gödel's argument needs at least **four** separable things — the same four
`R3-the-margin.md` §5.3 lists:

| Ingredient | Status |
| --- | --- |
| **(i) a fixed point** — and note §2.1's distinction: Lawvere/Kleene make *selected* diagonals cheap, and the one Willard adopts as Group-3 is the **self-consistency** diagonal `H ↔ Con(B+H,D)`, not the G2 diagonal `G ↔ ¬□G` | never removed — every system here keeps the diagonals it needs |
| **(ii) a derivability condition** | **Willard** removes one: (2) on the tableaux line, on his own statement (§2.3); (2) or (3) on the Hilbert line, by analogy (§2.3a). **Pakhomov** is silent on which, if any, `H_{<ω}` satisfies — he notes only that "HBL conditions do not necessary hold in the case of some weaker arithmetical c.e. theories" (p. 3) |
| **(iii) the semantic step** from an inconsistency-witness to falsity | **Pakhomov** blocks the step *after* the conditions: Pudlák's argument carries a failure of G2 in `T` into "a different theory `T′` (interpretable in `T`), where HBL are satisfied", and that interpretation needs successor totality, which `H_{<ω}` denies — the same door Willard uses. The finite models are his positive witness (§1) |
| **(iv) the structural licence** to merge two boxed derivations off one context | **Beklemishev–Shamkanov** remove it — but the resulting system is **not self-verifying**; only *formalized* G2 fails (§2.5a) |

**R6 sharpens this table and should be read with it.**
[`lawvere-sjas.md`](lawvere-sjas.md) §5 replaces the single "ingredient
removed" list with **two independent axes** — a structural diagonal on boxed
hypotheses, and uniform internal HBL evaluation — plus a separate axis for
fixed-point strength, and warns that they "should not be collapsed into a
single *weakness* ordering": Beklemishev–Shamkanov weaken structural logic
while keeping the modal derivability rules, Willard keeps structural logic and
weakens what the arithmetic can certify uniformly about its own proof
predicate. The table above is one projection of that square. R6's square omits
Pakhomov, declared with reasons at its §6.

So the honest count is: **two demonstrated routes to a self-verifying theory**
(Willard's, and Pakhomov's), plus **one demonstrated way to break the argument
without obtaining such a theory**. Row **(iv)** is a result about G2's proof,
not an entry in a catalogue of self-verifying systems, and
[`R3-the-margin.md`](R3-the-margin.md) §5 is corrected accordingly.

> **Refined reading of G2.** Gödel's Second Theorem is not a theorem about
> self-reference. It is a theorem about systems that meet *all* of its several
> prerequisites at once. Breaking any one defeats the *argument*; obtaining a
> self-verifying *theory* is a further and harder thing, and only two are known.

## 3. What the cost actually is

Take a proof `p` of length `L`. For the system to certify "`p` is a proof", it
must exhibit an object that encodes the whole verification. The classical
bound (`Willard2007-APAL` §4) is that this object has Gödel number below
`ω₁^K(p)` for some fixed `K`, where `ω₁(x) = x^{Log(x)}` and `ω₁^K` is `K`
iterations of it.

So the system needs to **name a number substantially larger than `p`**. If it
can, the diagonalisation closes and G2 applies. If it cannot, the argument
stalls — not because the fixed point is absent, but because the step from the
fixed point to the contradiction cannot be taken inside the system.

Willard's whole corpus is the exploration of systems too weak to take that step
while still strong enough to be interesting.

---

## 4. Naming rates, and why exactly two of them matter

The corpus's single most repeated object (codified §5.3; ten occurrences catalogued
in [`../codification/concordance/replication-map.md`](../codification/concordance/replication-map.md) §1,
the tenth added 2026-09-02 — the origin text `Willard1993-TR` printed p. 6, which
the map had omitted in favour of the 12-page published abbreviation)
is a pair of sequences from the same start:

| | recurrence | value | **encoding length** |
| --- | --- | --- | --- |
| additive | `x_i = x_{i−1} + x_{i−1}` | `x_n = 2^{n+1}` | `Log(x_n) = n + 1` |
| multiplicative | `y_i = y_{i−1} · y_{i−1}` | `y_n = 2^{2^n}` | `Log(y_n) = 2^n` |

Both are built in `n` steps. The additive sequence names a number whose binary
encoding is `n + 1` bits — **linear in the work done**. The multiplicative
sequence names a number whose encoding is `2^n` bits — **exponential in the
work done**.

That second fact is the whole game, and `Willard2009` Lemma 5 puts it inside a
proof rather than a sequence: `n` rounds of the elimination rules of Eqs. (43),
(45) and (48) build `U₀ = 2`, `U_{i+1} = U_i · U_i` along one **pivotal branch**
of a fragment `F` with only **`O(n)` nodes**, establishing `U_n = 2^{2^n}`,
"whose binary encoding has a `2^n` length that is much larger than `F`'s
length."

**Status: `sketch`.** Willard writes "We obviously have omitted many details
here". It is the most *legible* statement of the mechanism in the corpus, and it
is not a completed proof.

A short proof has named a long object. Once that is possible, the system can
assert — cheaply — the existence of something big enough to encode a refutation
of itself, and G2 bites.

**This is why the boundary sits where it does**, and Willard says so three
times, in three papers, each naming the specific lemma that dies when
multiplication stops being total (`Willard2000-TAB` §5, `Willard2002a` §7,
`Willard2004` p. 4; codified §7.2). The affirmative and negative halves of the
corpus are two readings of one observation: the same construction that lets a
system *be* self-justifying is what the incompleteness proofs *consume*.

---

## 5. The criterion

**The criterion is not a growth rate.** It is natural to try to state it as one —

> *(this form is refuted; see below)* A system is self-justifying precisely when its naming is
> **non-compressive**: no proof of length `L` may denote an integer whose binary
> encoding exceeds `O(L)`.

**The arithmetic refutes it.** Writing `Log₂ C_n` for the bit-length of what `n`
naming steps reach:

| Naming convention | `Log₂ C_n` | `O(L)`? | Willard's verdict |
| --- | --- | --- | --- |
| incremental `C_{i−1}+1` | `log n` | yes | works, but **not Continuously Expanding** — it reaches every integer, too slowly for the `K_i` sequence `Willard2006a` p. 7 requires |
| **additive** `C_{i−1}+C_{i−1}` | `n` | yes | **works** |
| **Hybrid(1)** `⌈2^{[Log i]}⌉·C_{i−1}`, `i ≥ 3` | `Θ(n log n)` | **no** | **works** |
| Hybrid(H), `H > 1` | `Θ(n (log n)^H)` | no | fails |
| multiplicative `C_{i−1}·C_{i−1}` | `2^n` | no | fails |

The third row is fatal: Hybrid(1) is a **positive** case and is not `O(L)`. And
the boundary Willard actually draws sits between `n log n` and `n (log n)²` — a
**single logarithmic factor** — which no asymptotic condition of that coarseness
can express. `Willard2006a`'s own gloss that Hybrid is "midway between the
additive and multiplicative conventions" is loose, and the `O(L)` form was built
on taking it as exact. Details at `R3-the-margin.md` §3.1.

**What survives is the shape, not the threshold.** The right statement is not a
rate at all but an **additive margin**, and Willard states it once —
`Willard2011` Definition 4.5 calls a configuration **Tight** when every
inconsistent `β` above the base satisfies

> `Log(q_β) ≥ ♯(β) + 2`

— refutation length at least envelope plus two. A rate condition compares
*growth*; this compares two *sizes at each `β`*, and a gap of two bits is far
finer than any `O(·)` class. That is why R3 takes the margin, not the rate, as
the definition:

> **Criterion (as R3 states it).** `M(ξ) = inf_β { Log(q_β) − ♯(β) }` over
> inconsistent `β ⊇ B^ξ`, and the system is self-justifying when `M(ξ) ≥ 2`.

Willard's informal ordering — the incremental convention "grows too slowly", the
multiplicative too fast, the additive "simultaneously *sufficiently slow* … while
also *sufficiently fast*" (`Willard2006a` pp. 7–8) — is then the margin's
behaviour at three points, not an independent criterion.

### 5.1 The five dials, and how far they can be unified

The codification records five parameterisations of the boundary that no paper
relates to any other (drift **D38**; codified §8.1): `ℜ`, Level(n), Hybrid(H),
`Herb−k`, and `Z`. **Three** of them are now *proved* to move the margin, one is
undetermined, and one has been retracted.

**The split that works is apparatus versus sentences**, and it rests on a
one-line lemma ([`R3-the-margin.md`](R3-the-margin.md) §3.0): `Willard2011`
Defs. 4.1–4.4 define `♯` entirely by truth in the standard model — `E(n)`
denotes the fixed value `2^n`, `Scope_E` is a syntactic operation, `Good(N)` is
a semantic condition — and **none of them mentions the deduction method**. So a
dial that varies only `d` cannot move `♯(β)`, and whatever it does to
`Log(q_β)` it does to the margin.

- **`ℜ`** (`Willard2004`) and **`Herb−k`** (`Willard2009` Def. 7) widen the class
  of intermediate theorems a proof may use — a property of the apparatus. That is
  cut, hence proof compression: `Willard2002a` Theorem 2.2 (**`sketch`**) composes proofs of
  `Λ`, `Θ` and `Λ∧Θ ⇒ Ξ` into one of `Ξ`, and under a cut-free method the
  composite's "length can certainly be super-exponentially longer than the
  combined lengths of `p`, `q` and `r`", its existence assured only by Gentzen's
  Cut Elimination Theorem. (An earlier gloss said "cut elimination preserves
  theoremhood while the proof can be longer", which misplaces the blowup: it is
  in building the composite, not in eliminating cuts from one already built.
  §2.3 states it correctly.) **Proved.**
- **`Z`** (`Willard2020`) fixes which `Υ ∨ ¬Υ` instances are *logical axioms* —
  again the apparatus — and LEM-as-axioms is a cut (codified §4.5). **Proved.**
- **Hybrid(H)** varies the language and base, so both terms are in play and the
  lemma does not apply. Which term it moves is **undetermined** (R3 §3.1). It is
  not the envelope: `♯` is model-theoretic and the naming axioms are true, hence
  `Good(∞)`, so they cannot lower `♯(β)` — which is Willard's own argument for
  Eq. (20), not this Refinement's.
- **Level(n)** varies the self-referential axiom `Ψ` — a *sentence*. Its instance
  is **retracted** (R3 §3.3): Group-3 is not part of `B^ξ` but added to it, and
  at the critical `β = B^ξ + Ψ` Willard's Eq. (20) makes `Log(q)` and `♯(β)` move
  in **exact lockstep**, `Log(q) = ♯(β) + 1`, so the margin there is identically
  1 whatever the level.

One caution that survives unchanged: **`ℜ` and `Z` are grouped by mechanism, not
identified.** Codified §8.1 expressly forbids the merge — `ℜ` parameterises
intermediate theorems, `Z` parameterises logical axioms, and whether a result
transfers between them is the corpus's own open problem (drift **D34**,
obligation **O46**, gap **G22**). Their safe settings are not even at the same
level: `Π*₁ ∪ Σ*₁` versus `Δ*₀`.

So the claim is **one inequality, three dials provably writing on its cost term,
one unplaced and one retracted** — not that the five dials are one dial.

---

## 6. The Rosser analogue

Rosser's improvement to Gödel replaces "I am not provable" with

> `∀y ( Prf(⌜R⌝, y) → ∃z < y  Prf(⌜¬R⌝, z) )`

— written in the corpus's argument order, **theorem first, proof second**, to
match `SemPrf` below and `Willard2020` Eq. (5)'s `Prf_{IS_D(β)}(x,p)`;
textbook presentations of Rosser usually write the proof first. It is a
sentence that does not assert unprovability absolutely, but **relative to an
ordering by proof size**. That single change buys the theorem for merely
consistent systems.

Willard's construction makes the same move, in the other direction.

**The sentence is bounded.** The working form of the diagonal throughout the
corpus is not `Prf` but `SemPrf^K_α(x, y, z)` — "`y` is a proof of `x` **and**
`y` is bounded by the `K`-fold iterated logarithm of `z`". The self-reference is
to the non-existence of a **short** proof, not of any proof.

The two definitions differ in strictness, which no paper notes: `Willard2000-TAB`
Definition 1 requires **`y < Log(z, K)`** and `Willard2007-APAL` Definition 2
requires **`y ≤ Log(z, K)`** (drift **D81**). Nothing in either argument turns on
it, but a refinement that unified them silently would be doing the thing this
project exists to avoid.

**The argument is a least-number principle on size — in one of the two methods.**
It is not *every* consistency-preservation proof in the corpus, and codified §6.9
says so in its first sentence: "The Hilbert line does not use `θ`-Compactification. Its preservation
results are proved directly, **by minimal counterexample**." There are two
engines.

- **The least-number method**: assume a proof `P` of `⊥`, take the one
  minimising the Gödel number `Θ(P)`, construct a smaller `R`, contradict
  minimality. `Willard2001` Thm 3.4 (`ISREF`) and Thm 4.3 (`IS`), `Willard2006a`
  Thm 3 (`ISCE`), `Willard2016` Thm 6.7 (`IQFS`, conditional on Conj. 6.6) —
  codified §6.9.
- **The `θ`-compactification method**: `Willard2005` **Theorem 1** (codified
  §6.4) — if `D` is `θ`-Compactified for some `θ < 1/3` then `IS_D(·)` is
  consistency preserving. **Apparatus-neutral**, saying nothing about tableaux,
  and pushing everything apparatus-specific into the one hypothesis.

**The two methods do not partition by line.** `Willard2001` Thm 4.3 is about
**`IS(A)`**, which is *semantic tableaux* (registry `SJAS-Origin`; codified
§3.1's Type-A row) — so a tableaux system's preservation is proved by minimal
counterexample in 2001 and covered again by the apparatus-neutral engine in 2005.
The division is between **proof methods**, not between the two families of
system; codified §6.9's title, "The Hilbert line's engine", is looser than its
own table.

The size-relativity that makes the Rosser parallel is present in both methods,
but it lives in different places: in the minimised `Θ(P)` in the first, and in
`U-Height(p) < (1/5)·Log₂(p)` and the `θ` bound in the second. The parallel below
is stated for both; the least-number *mechanism* belongs to the first method,
whichever family of system it is applied to.

So:

> **Rosser bounds the diagonal by comparison; Willard bounds it by magnitude.**
> Both replace an absolute self-referential claim with a size-relative one.
> Rosser buys G2 for consistent systems. Willard buys self-justification for
> systems that cannot count high enough to refute themselves.

The recorded hook asked whether the consistency-preservation results could be
*derived from* a Rosser analogue. The honest answer from the codified material
is that they already **are** one — the trick is present, undeclared, in the
`SemPrf^K` predicate and in the minimal-`Θ` argument. What the Refinement adds
is the observation that this is what they are.

---

## 7. What the idea costs

A refinement that stated only the mechanism would be a misrepresentation. **Five
things are given up.** Willard states the first four himself (codified §1.4);
the fifth is Pakhomov's, from outside the corpus.

1. **Ordinary arithmetic is gone.** Every affirmative system is Type-A (no
   multiplication as a total function) or Type-NS (nothing total). `Willard2005`
   **Remark 4** — a remark, not a theorem; the row carries no proof status —
   states the engine is impossible once multiplication is admitted. Its wording
   matters: the strong half is *attributed*, not proved there —

   > "a Level(0-) tableaux generalization **in [68] implies that** … no useful
   > analog of Definition 5 can be found for axiom systems recognizing
   > multiplication as a total function, under any possible deduction method
   > `D`, whether cut-free or otherwise"

   The corpus's strongest apparatus-independent negative statement is thus
   carried by a citation, and this Refinement does not upgrade it.
2. **The proof is one line.** `Willard2011` Remark 6.16b: self-justification
   from a SelfCons axiom yields "essentially a **1-line proof**" — instinctive
   faith, not proof-justification.
3. **The programme's own verdict.** `Willard2016` §9, quoted whole because the
   qualifier does real work: "All our published articles about self-justifying arithmetics
   have emphasized that evasions of the Second Incompleteness Effect rested on
   using arithmetics that were **weaker than traditional arithmetics in, at
   least, some respects**. (The Second Incompleteness Theorem's significance in
   refuting the original objectives of Hilbert's Consistency Program is thus,
   simply, undeniable.)" The qualifier *in, at least, some respects* is
   Willard's, and was elided.
4. **The objection stands, and the reply to it is conditional.** `Willard2016`
   §8 states the objection in Willard's own voice — "Is it not *almost cheating*
   when an axiom system verifies its own consistency by using ⊕'s formalized
   'I am consistent' axiom as an intermediate step…? After all, such a technique
   can verify its own consistency only in a *technically purely legalistic
   sense*" — and answers it with Definition 8.1's **Platonic Stability** and
   Corollary 8.2: adequate Platonic Stability lets a thinker presume its own
   consistency and "rest assured the assumption will not drive `IQFS(β)` into
   inconsistency".

   **But Corollary 8.2 depends on Theorem 6.7, which is conditional on
   Conjecture 6.6** — `stated-only` — so Willard's answer to his own strongest
   objection is itself conditional. Obligation **O66** requires that be flagged
   *wherever the result is used*, not only where it is introduced.

5. **The axioms are not natural.** Pakhomov's assessment, from outside the
   corpus: Willard's theories "are not completely natural in the sense that
   **some of axioms are constructed using Diagonal Lemma**". His `H_{<ω}` is
   offered explicitly as "a more natural example of this kind". That is a fair
   charge — the Group-3 axiom *is* a fixed point, by construction (§2) — and it
   is the sharpest external criticism of the programme on record.

The refined claim is therefore **not** that Hilbert's programme survives. It is
that the reason it fails is narrower and more interesting than "self-reference
is paradoxical": it fails because **checking your own work costs more than
doing it**, and any system cheap enough to escape that cost is too weak to be
the arithmetic Hilbert wanted.

---

## 8. The same phenomenon outside arithmetic

The synthesis names computational analogues as the near-term purpose of this
work. There is a worked instance in the held literature. What transfers is the
**shape** of §5's criterion — keep the fixed point, deny the derivation — not
its arithmetic form, since §5's rate condition has been withdrawn and its
replacement, the margin, is stated in bits of Gödel number.

**Self-interpreters for total languages.** Folklore holds that a strongly
normalizing language cannot have a self-interpreter: a total `eval` of the
obvious type permits a diagonal argument. Brown and Palsberg
(`../../lit/brownpalsberg2016self-interpreter-f-omega.pdf`, and the System U and
typed-self-evaluation papers alongside it) nevertheless construct one for
**F_ω**.

**The shape is the same as Willard's; the mechanism is not.** Willard blocks by
cost, Brown–Palsberg by typing — two mechanisms with one shape, which is why the
transfer below is an analogy rather than a correspondence.

The shape is this. The self-interpreter exists and is total — the
self-representation is not what fails. What fails is the *paradox's*
machinery: in their own words, "**static type checking in F_ω can exclude the
proof's diagonalization gadget**, leaving open the possibility for a
self-interpreter" (abstract, image-verified). The diagonalization **gadget**
is exactly what static typing excludes; what survives is the
self-representation, not the gadget built from it. The fixed point survives;
the derivation does not.

In the vocabulary of §2: Lawvere's existence half is kept, and the
derivability-condition half is denied — there by typing discipline, in Willard
by growth restriction. Both are instances of

> keep the self-reference; make the *use* of it unaffordable.

That **analogy** — it is not yet a correspondence, since no formal mapping has
been established — is the concrete form of the "computational analogue" the
synthesis is aiming at, and it suggests the transfer to test first: **the margin should have a
type-theoretic reading, in which the two terms become the size of the type a
term can denote and the size of the term denoting it.** That is a conjecture of
this Refinement, not a result.

**R6 gives R4 a sharper target than this conjecture.** Rather than testing a
type theory against "self-reference" as a whole,
[`lawvere-sjas.md`](lawvere-sjas.md) exhibits the boxed diagonal `copy_A : □A
→ □A ⊗ □A` and the uniform proof-composition map as **separate** data, so each
can be checked independently — which is exactly the distinction
Brown–Palsberg's typing discipline turns on. ADR-0004 records it as a
consequence for R4.

R4 also carries obligation **RO1**: `□`-contraction can hold in affine PA
(Beklemishev–Shamkanov §3), so contraction-freedom at the object level does
not by itself buy anything, and the affine-tree line must check the
*restricted* rule rather than the general one.

---

## 9. What is not settled

**The Beklemishev comparison is permanently blocked — not pending.** The
synthesis takes its inspiration from "Lev Beklemishev's claim of a simplified
presentation of SJAS". **There is no such survey**: Pakhomov 2019 footnote 3
records the simplification as "still unpublished", and no later publication is
evident. **G7 is closed `accepted`** ([`R1-review.md`](R1-review.md) §1) and
acceptance criterion B4 is replaced by **B4′**. Everything above is an
independent identification, and there is nothing in the published record to
reconcile it with.

**The secondary-literature pass has not been done, and one gap in it is
load-bearing.** ADR-0002's **R5** — Artemov, Pudlák, Visser, Adamowicz,
Kołodziejczyk, Salehi — is pending. Artemov, Visser and Salehi are held in
`../works-citing-dew/`; Adamowicz and Kołodziejczyk are not.

**Pudlák is not held at all** (gap **G39**, opened 2026-09-04). That matters
here more than the others, because the claim that *his cut-shortening
technique requires successor totality* is load-bearing in this document — it
is what §1 and §2.5b use to explain how both Willard's Hilbert line and
Pakhomov's `H_{<ω}` evade him. Every one of those invocations rests on
**Willard's or Pakhomov's description of the paper**, not on the paper. The
descriptions agree with each other and there is no reason to doubt them; but
"agrees with the two authors who need it to be true" is a weaker warrant than
this stage applies to Willard's own corpus, and the asymmetry should be
visible rather than silent. Until Pudlák is acquired, every claim here about
what his proof requires is a claim about what Willard and Pakhomov say it
requires.

**The Lawvere framing was a framing; R6 has since made it a construction —
though not a proof of a Willard result.** §2 uses Lawvere to separate
*existence of a fixed point* from *derivability of its consequences*, which is
correct and clarifying. It is no longer only a framing: **R6 has done the
construction**: [`lawvere-sjas.md`](lawvere-sjas.md), charter
[`ADR-0004-lawvere-sjas-translation.md`](ADR-0004-lawvere-sjas-translation.md).

R6 builds a common coded modal proof doctrine, presents the initial arithmetic
universe (van Dijk–Oldenziel), Beklemishev–Shamkanov and Willard in it, and
isolates the *missing morphism* in each case. Its Theorem 1.1 adds a coordinate
this document does not have: beyond a fixed point and the three HBL conditions,
formalized G2 also needs a **boxed diagonal** `copy_A : □A → □A ⊗ □A`, and
B–S's calculus is symmetric monoidal rather than cartesian precisely there.

What remains true is the *disclaimer*: R6 explicitly does **not** claim `IS(A)`
is an arithmetic universe, does not make `A ↦ □A` an endofunctor in Willard's
doctrine, and does not show that a blocked derivation is by itself sufficient
for self-justification. Its own §6 lists those non-claims, and its AU source's
§6 Löb derivation is sketch-level. So: a construction, honestly bounded — not a
categorical proof of a Willard theorem.

**The criterion of §5 now has a definition — see
[`R3-the-margin.md`](R3-the-margin.md), whose §6 carries the verdict.** The
**margin** `M(ξ) = inf{ Log(q_β) − ♯(β) }` **is** `Willard2011` Definition
4.5, restated as an infimum. It does not *generalise* Def. 4.5: `∀β. A(β) ≥ 2`
and `inf_β A(β) ≥ 2` are the same statement.

**And one status governs the whole construction.** `Willard2011` **Lemma 4.6** —
Tightness implies `B^ξ + SelfRef` is a consistent self-justifying system, which
is what makes the margin worth defining — is **`sketch`**, its proof "kept brief
and informal" and deferred to §5, where the rigorous engine (Thm 5.9, `full`)
consumes **EA-stability instead of Tightness**. The margin's sufficiency has no
`full` proof anywhere in the corpus.

**And what it certifies is the weaker form of self-justification.** Lemma 4.6
delivers `B^ξ + SelfRef`, which asserts only that no proof of `0=1` exists.
Willard's own engines deliver `B^ξ + SelfCons^k`, which denies *simultaneous*
proofs of a `Π^ξ_k` sentence and its negation — and Remark 5.8 (printed p. 17)
says the latter is "**significantly stronger**" than the former, "even when
`k = 1`". So the margin, as the corpus supports it, certifies less than
Willard's stability engines do. R3 §§2.2 and 6 carry both points.

**Three of the five dials — `ℜ`, `Herb−k`, `Z` — are now proved** to lower the
refutation cost, because each varies only the deduction method and `♯` provably
ignores it (§5.1). **Hybrid(H) is unplaced.** **Level(n)'s instance is
retracted**: it varies a sentence, and at the critical `β` both terms move in
lockstep. D38's request is answered for three, sharpened for one, and reopened
for one.

**What R3 found that this document had not.** The threshold `2` is not a tuning
constant. When the self-referential axiom is false, `Willard2011` Eqs. (19)–(20)
give `Log(q) = ♯(B^ξ + Ψ) + 1` **exactly** — the fixed point sits at margin
precisely one — so Tightness's `+2` is the demand that every inconsistent `β`
have strictly more margin than the fixed point achieves, and Lemma 4.6 is that
one-line contradiction. This is the sharpest statement of the idea in the
corpus, and §5 above still presents the threshold as given.

**And one claim about novelty is withdrawn — then narrowed again.** R3's route
from A-stability to Tightness is sound and completes, and a conclusion of its
shape is not new: Remark 5.2 announces that "Theorem 5.11 will show the presence
of A-stability, alone, is sufficient for constructing self-justifying systems".
But it does not *re-derive* Theorem 5.11 either, because 5.11 reaches
`B^ξ + SelfCons⁰` and R3's chain reaches only `B^ξ + SelfRef`. It is a shorter
route to a weaker destination — a simplification in reach as well as in length,
which is a smaller thing than a simplification of the same result.

The instances that stand are **argued from Willard's results for two of the
three, and proved for all three only in the weaker sense that the term they move
is identified** — none is derived from the inequality by computing where it
flips. And the wider unification is **refuted**, though not for the reason first
given: the margin is quantitative on a fixed logic with fixed semantics, so it
does not reach Beklemishev–Shamkanov, who vary the structural rules — and whose
system, by their own §6, is **not self-verifying** at all. Pakhomov is a second
*witness kind* within the same evasion of Pudlák, not a system outside the
margin's reach. Self-verification is a classification of G2's failure modes, and
breaking the argument is necessary but not sufficient for it.

**Most of what the refinement leans on is not `full`.** Per codified §8.1, only
**one** of the five dials (Level(n)) is `full` on both settings: `ℜ` is
`sketch`/`sketch` (`Willard2004` Thms 3 and 2, gap **G35**, permanently sketch),
Hybrid(H) and `Herb−k` are `stated-only` on both halves, and `Z` is
`stated-only` with its `Π*₁` case **open**. `Willard2016` Theorem 6.7 is
conditional on Conjecture 6.6, and `Willard2009` Lemma 5 — §4's mechanism — is
`sketch`.

**And note the awkward join.** The one dial that is `full` on both settings is
Level(n) — the very one whose margin instance §5.1 has just retracted. The three
dials the margin *can* place are the three whose Willard-side results are
weakest: `ℜ` at `sketch`/`sketch`, `Herb−k` and `Z` at `stated-only`. §5.1 is as
strong as these statuses allow and no stronger, and that is not very strong.

**Every quotation in this document is now image-verified.** A review on
2026-09-02 found nine that had no row in [`VERIFICATION.md`](VERIFICATION.md);
all nine were then read against the page and registered. Eight were accurate.
**One was not**: §7's report of `Willard2016` §9 quoted "weaker than traditional
arithmetics" and dropped Willard's own qualifier *in, at least, some respects*,
making his concession sound absolute. §7 now carries the sentence whole.

That is the third elision of this kind the project has caught, and it is worth
naming the pattern: the failures are not mistranscribed symbols but **dropped
hedges and dropped attributions** — `Willard2005` Remark 4's "in [68] implies
that", `Willard2020` Thm 4.5's hypothesis on `β`, and now this. Extracted text
was never the cause; compression was.

Check R-B validates the register's rows and cannot detect a quotation that has
no row, so register completeness remains a human obligation, like verbatim
transcription before it (ADR-0002 **B6**).
