# Discussion record: the 2011 engine machinery, and Rosser comparison

Date: 2026-08-21. Context: the C6 checkpoint. Recorded per the parent
repository's documentation practice (longer conversational notes live as their
own file and are linked from the LOG), scoped into `nachlass/` per the
codification working agreements.

Referenced from [`../../LOG.md`](../../LOG.md).

---

## Summary

A five-exchange clarification of `Willard2011`'s engine, followed by a
comparison with Rosser's Trick and two corrections. What it settled:

1. **What the unification actually abstracts.** Definition 3.4's 5-tuple is not
   bookkeeping — it is exactly the data the engine argument (Theorem 5.9) never
   touches. Tracing that proof shows it consumes only `♯`, minimality, and the
   two stability invariants; nothing about U-Grounding functions, tableau
   rules, or coding appears until Appendix D establishes stability per
   configuration. The abstraction therefore identifies an *interface*.
2. **Where the simplification lives.** 2005's ending is an arithmetic
   comparison between two different envelope magnitudes (`(m−1)/m^θ` versus
   `m^θ`), which is why θ is constrained and why the hidden `m ≥ 3` side
   condition arises. 2011's ending is a definitional impossibility: both a
   sentence and its negation land on the *same* `Good{½♯(Φ)}`. That is why the
   constant ½ can be fixed — it does no arithmetic work — and why obligation
   **O1 is 2005-only**.
3. **What `♯` is.** `Scope_E(Υ,N)` rebounds unbounded quantifiers by `E(N)=2^N`,
   making `Good(N)` a *decidable* approximation to Υ's truth. For Π^ξ₁ Υ the
   good set is downward closed, so `♯(Υ)` is a genuine threshold: **distance to
   the first counterexample, in bits**, with `♯ = ∞ ⟺ Υ true`. Truth is the
   limiting case of a graded, log-scale quantity — which is what lets it
   collide with proof lengths. For the SelfCons sentence the counterexample
   *is* a pair of proofs, so one number is simultaneously semantic and
   syntactic. That coincidence is the mechanism.
4. **Why stability quantifies over false R-Views.** Willard's stated reason is
   effectiveness (no recursive procedure isolates the true Π^ξ₁ sentences), but
   the real one is technical necessity: Theorem 5.9 applies A-/E-stability to
   θ = Φ's one-sentence view *after assuming inconsistency*, so θ is false. The
   guarantee reads: if your proof is shorter than the distance to your
   premises' first error, your conclusion is still reliable out to half that
   distance. Appendix F's Epistemological Bundle Theory is the same fact read
   epistemologically.
5. **Why a sentence and its negation cannot be Good at one level.** Because
   `Scope_E` bounds both by the same `E(N)` and leaves the matrix alone, the
   two bounded statements are exact contradictories over a common finite
   domain. The Π and Σ polarities run opposite ways (footnote 6), which is what
   makes "Good at the same level" contradictory rather than redundant, and why
   two stability hypotheses are needed rather than one.
6. **Rosser comparison.** Real parallels (bounded comparison, decidability,
   minimality) but a decisive disanalogy: Rosser's comparison lives *inside the
   object sentence*, Willard's lives *in the metatheory* — `SelfCons_k`
   contains no length comparison at all. Rosser-style self-verification is the
   Feferman-style "intensionally incorrect" route Willard explicitly rules out.
   Both break the same derivability condition (provable modus ponens) for
   opposite reasons. Evidence: Willard cites Rosser only historically, and
   carries Arai's *Derivability Conditions on Rosser's Proof Predicates* and
   Švejdar's *Modal Analysis of Rosser Sentences* in both spine bibliographies
   **without engaging either in the body** — so the Refinement-stage comparison
   is open, not foreclosed.

Artefacts produced: erratum **E6** (`willard2005.md` §7), obligations **O26**
(nothing is redefined — see the correction at the end of the transcript) and
**O27** (the syntactic-versus-semantic negation delicacy).

---

## Verbatim transcript

Tool invocations are noted in brackets where they interrupted the prose; the
prose is reproduced as written.

---

**User:**

> Please elaborate on the "Unification is real" and "Simplification is real and locatable" findings.

**Assistant:**

Both findings are more substantive than the summary conveys. Taking them in turn.

### Unification: the abstraction identifies an interface

The mechanical description — "2011 turns 2005's fixed choices into parameters" — undersells what Definition 3.4 does. The interesting question is *which* five things became parameters, and the answer is: exactly the things the engine argument never touches.

Trace what Theorem 5.9's proof actually consumes. Assume `S = B^ξ + SelfCons₁(B^ξ,d)` is inconsistent. Then Φ (the SelfCons sentence) is false in the standard model, so `♯(Φ) < ∞`; a minimal counterexample tuple exists with `Log{Max[p̄,q̄,x̄,ȳ]} = ♯(Φ)+1`; A-stability sends the Π side and E-stability the Σ side to `Good{½♯(Φ)}`; and a sentence and its negation cannot both sit at one goodness level. That is the whole proof. **Nothing in it mentions U-Grounding functions, tableaux rules, or a coding scheme.** Those appear only in *establishing* stability — which is Appendix D's job, done once per configuration.

So the 5-tuple isn't bookkeeping; it is the minimal data that must be fixed before "stable" is even a well-formed predicate, and the engine is proved against that interface rather than against any implementation of it. The proof that this is the right cut is empirical and striking: the same engine covers semantic tableaux (ξ\*), a restricted modus ponens (ξ\*\*), **unrestricted Hilbert deduction** (ξ⁻), and Herbrand-style Skolemization (ξ^R). An argument that survives that range was never really about the apparatus — it was always about the interaction between proof size and reachable envelope. That is a genuine payoff for the Refinement stage, which wants the essential idea rather than Willard's presentation of it.

Two details make the abstraction sharper than it first appears:

**The coding method is a component.** In 2005 the encoding is a construction (Appendix A's B-adic scheme) and Eq. (20) is an assertion *about* it, extended by the aside that "any other reasonable encoding methodology" also satisfies it. In 2011 `g` is a parameter of ξ and the demand on it is a stated **condition** — Gödel number ≥ 32^J for a proof with J function symbols. The hand-wave becomes a checkable side condition, which is what lets "any natural mechanism is equally suitable" carry weight.

**`♯` is intrinsic where `Normed(a,b)` was relational.** 2005's hypothesis attaches two chosen envelope parameters `(a,b)` to a system from outside; 2011's `♯(θ)` is *computed from* θ. The hypothesis becomes self-contained, which is precisely what allows Lemma 6.1 (adding true Π^ξ₁ sentences preserves stability) to be stated and proved in a few lines — there are no parameters to re-fit.

The cost, which the audit records: 2011 quantifies stability over **all** R-Views in `RE-Class(ξ)`, including sets of sentences that are *false*. That is a demanding hypothesis, and Remark 5.2 shows Willard knew it was interesting in its own right — it says short proofs from a technically inconsistent view still yield "partially useful" theorems.

### Simplification: where it happens, and what it costs

The most illuminating part is a change in the *character* of the final step, not just its length.

**2005's ending is an arithmetic comparison.** Φ comes out `App∀((m−1)/m^θ)`-valid and ¬Φ comes out `App∃(m^θ)`-valid — two *different* envelope magnitudes. To close, you need `(m−1)/m^θ > m^θ`, i.e. `m − 1 > m^{2θ}`. That is why θ must be constrained (`< 1/3`), and why the step carries the hidden `m ≥ 3` side condition that became composition obligation O1.

**2011's ending is a definitional impossibility.** By routing the Π side through A-stability and the Σ side through E-stability — two separate hypotheses deliberately tuned to deliver the *same* level — both Υ and ¬Υ land on `Good{½♯(Φ)}`, and the contradiction is that a sentence and its negation cannot both be good at one level. No magnitudes are compared.

That reframing is why the constant can be **fixed at ½** rather than left as a free exponent: ½ is no longer doing arithmetic work, it only has to be *the same on both sides*. And it is why 2011 needs no analogue of the `m ≥ 3` condition — which is exactly the scoping that lets me mark O1 as a 2005-only obligation rather than something the codified statement must carry generally.

The rest of the simplification is consolidation with a clear ledger:

| 2005 | 2011 |
|---|---|
| `App∀(a)` and `App∃(b)` — two conditions, two parameters | `Scope_E` + `Good(N)` + `♯` — one operator, one intrinsic measure |
| Definition 5, one definition with parts (A) and (B) and a free θ | Definitions 5.1 and 5.3 — two *named* properties, constant ½ |
| Theorem 3 (tableaux) and Theorem 5 (Tab−U\*₁), each with its own compactification theorem | Theorem 6.3, one theorem parameterized by stability class |

**What it costs.** The saving is conceptual factoring, not economy. The per-apparatus combinatorics does not disappear — 2005's nine-case Positive-Branch induction reappears as Appendix D-2's four-page proof of Theorem D.4, and must be redone for each new configuration. The paper is *longer* than 2005 (64 pages against 43), because it must build the abstraction, instantiate it four times, and prove the new extension results. Per paradigm it is far more economical; in absolute terms it is not.

Willard's own framing in Remark 5.12 is narrower than the title suggests: proofs by contradiction "are notorious in the mathematical literature for being confusing. They should be simplified whenever possible. This has been done mainly through Theorem 5.9's short proof." He is claiming *visualizability*, not brevity — which matches what the comparison shows.

One structural gain worth naming, because it has no 2005 counterpart: the stability hierarchy maps cleanly onto the consistency level achieved. EA-stability buys **Level(1)**; any one of A-, E-, or 0-stability buys **Level(0)**. In 2005 the Level(0-)/Level(1) distinction existed but was tied to which paper's system you were reading; in 2011 it is a theorem-level correspondence between a hypothesis class and a conclusion strength. That is the kind of structure the result matrix at C13 will be built on.

---

**User:**

> 0) Please elaborate on "Then Φ (the SelfCons sentence) is false in the standard model, so ♯(Φ) < ∞" - what is  ♯(θ), and how does it allow identification between a numerical bound and truth/falsity? 1) Please elaborate on "2011 quantifies stability over all R-Views" (including sets of sentences that are false). 2) Why can a sentence and its negation not both be "good at one [the same] level"?

**Assistant:**

Three questions that go to the heart of the machinery. Taking them in order.

#### 0) What `♯` is, and how a number encodes truth

The link runs through **Definition 4.2**. For a prenex Υ, `Scope_E(Υ,N)` is Υ with every *unbounded* quantifier rebounded by `E(N) = 2^N`; bounded quantifiers inside the Δ^ξ₀ matrix are untouched. **Definition 4.3**: Υ is `Good(N)` when `Scope_E(Υ,N)` is true in the standard model.

The point is that `Scope_E(Υ,N)` is a **fully bounded, hence decidable** statement, whereas Υ is not. So Good(N) is a finite approximation to Υ's truth, indexed by how far out you look.

Now take Υ a Π^ξ₁ sentence, `∀v̄ φ(v̄)` with φ ∈ Δ^ξ₀:

- `Scope_E(Υ,N) = ∀v̄ < E(N) φ(v̄)` — "no counterexample below 2^N".
- Raising N *strengthens* the claim, so Good is monotone **decreasing** in N.
- If Υ is **true**, no counterexample exists at any height, so Good(N) holds for every N.
- If Υ is **false**, there is a least counterexample, and Good(N) holds exactly up to the point where that counterexample comes into range.

**Definition 4.4** takes the threshold: `♯(Υ)` = the largest J with Υ Good(J), and ∞ if all J. So

> **`♯(Υ)` is the distance to Υ's first counterexample, measured in bits**, and `♯(Υ) = ∞ ⟺ Υ is true`.

Truth is not *identified with* a bound; it is the **limiting case** of a graded quantity. That is what makes the whole apparatus work: falsity stops being a yes/no verdict and becomes a *magnitude on the log₂ scale* — the same scale on which Gödel numbers of proofs live. Once both are numbers, they can collide.

Willard makes the correspondence exact in footnote 10 of Theorem 5.9: with `L` the minimum of `Log{Max[p,q,x,y]}` over counterexample tuples, "Φ satisfies Good(L−1) but not Good(L)", i.e. `♯(Φ) = L − 1`. That is Equation (24): `Log{Max[p̄,q̄,x̄,ȳ]} = ♯(Φ) + 1`.

**The self-referential hinge.** For the SelfCons sentence

```
Φ  =  ∀x∀y∀p∀q ¬{ Neg₁(x,y) ∧ Prf_S(x,p) ∧ Prf_S(y,q) }
```

a counterexample *is a pair of proofs from S of a sentence and its negation*. So if `S` is inconsistent such a quadruple exists, Φ is false, and `♯(Φ) < ∞`. But now the same number is doing double duty: **semantically** it says where Φ's truth fails, and **syntactically** it bounds how short the shortest inconsistency proof can be. That coincidence — one quantity measuring both — is the entire mechanism. Compare 1993's cruder ancestor, where the same job was done by "the proof `p` consists of only log₂p bits" plus a doubling bound on parameter values.

#### 1) Why stability is required against *false* views

**Definition (§5).** An **R-View** θ is any r.e. set of Π^ξ₁ sentences; `RE-Class(ξ)` is all of them. Willard's stated reason is effectiveness: "An R-View does not need to be valid under the Standard-M model. It only needs to be r.e. … We choose this unrestricted definition because **no recursive decision procedure can identify all the true Π^ξ₁ sentences**." A hypothesis restricted to true views could not be applied to an arbitrary r.e. set, which is what the constructions actually hand it.

But there is a much better reason, and it is *technical necessity*, not generality.

Look at where Theorem 5.9 applies A- and E-stability. It sets **θ = Φ's one-sentence R-View** — and at that moment the proof has *already assumed S is inconsistent*, so Φ is **false**. The whole argument consists of applying a reliability guarantee to a premise known to be false and extracting a contradiction from the fact that the guarantee still delivers something. **If stability were only required for true θ, Theorem 5.9 would not even be applicable to its own proof.**

What the guarantee says, read plainly: for a false θ, `♯(θ)` is finite and measures how far out θ's first error lies. A-stability then says a Π^ξ₁ theorem proved from `θ ∪ B^ξ` by a proof with `Log(p) ≤ ♯(θ)+1` is still `Good{½♯(θ)}`. In words —

> **if your proof is shorter than the distance to your premises' first error, your conclusion remains reliable out to half that distance.**

The errors have not had room to propagate into short proofs. Remark 5.2 says exactly this: short proofs "produce at least *partially useful* deductions … *irregardless of whether or not θ's axioms are technically true*", and adds that this "makes the study of A-stability very interesting unto itself, apart from its applications in the current article."

Appendix F develops it as the **Epistemological Bundle Theory**: an agent walking through `RE-Class(ξ)`'s "bundled universe of valid and invalid collections of Π^ξ₁ sentences", where A-/E-stability formalize how it "can view short proofs from a technically inconsistent axiom system as containing pragmatically useful information under the assumption that the lengths of its proofs are shorter than the errors" — and Willard closes the loop: "those same invariants … which tempt a thinking agent to engage in a trial-and-error walk … also make viable Theorem 5.9's self-justifying formalisms." The tolerance of error and the self-justification are the same fact.

This is continuous with 2005, incidentally: `Normed(a,b)` never required truth either, and Theorem 1 shows `IS_D(A)` is `Normed(m−1,1)` precisely *under* the inconsistency assumption. 2011 names the class and makes the "possibly false" explicit.

#### 2) Why a sentence and its negation cannot be Good at one level

Because `Scope_E` bounds both by the *same* `E(N)` and leaves the matrix alone, the two bounded statements are exact contradictories over a common finite domain.

Let Υ be Π^ξ₁, so `Υ = ∀v̄ φ(v̄)`, and let ¬Υ be the corresponding Σ^ξ₁ sentence `∃v̄ ¬φ(v̄)`. Then:

| | unfolds to | says |
|---|---|---|
| Υ is Good(N) | `∀v̄ < E(N) φ(v̄)` | every tuple below 2^N satisfies φ |
| ¬Υ is Good(N) | `∃v̄ < E(N) ¬φ(v̄)` | some tuple below 2^N fails φ |

Both are decidable statements about the same finite range, and they are direct negations of one another. In the standard model exactly one holds. No arithmetic is involved — it is the excluded middle applied to a bounded domain.

Two things make this land where it does:

**The Π/Σ pairing is not incidental.** `Neg₁(x,y)` in the SelfCons sentence says `x` codes a Π^ξ₁ sentence and `y` codes *its negation*, which is Σ^ξ₁. So the two sentences the inconsistency exhibits are automatically one of each type — which is exactly why **two** stability hypotheses are needed, A-stability for the Π side and E-stability for the Σ side, and why `EA-stable` (both) is what Level-1 self-justification costs.

**The constant ½ is doing no arithmetic.** Both invariants are tuned to deliver `Good{½♯(Φ)}` — the *same* level. That is the whole role of ½: not to make a magnitude comparison come out right, but to be identical on both sides so the contradictories meet. Contrast 2005, where Φ arrives at `App∀((m−1)/m^θ)` and ¬Φ at `App∃(m^θ)` — two *different* magnitudes — so closure requires `m − 1 > m^{2θ}`, which is why θ must be constrained below 1/3 and why the hidden `m ≥ 3` side condition (obligation O1) appears there and nowhere in 2011.

One honesty note: `Scope_E(¬Υ,N)` is not the *syntactic* negation of `Scope_E(Υ,N)` — the rebounding is applied to each sentence's own prenex form. For a Π^ξ₁ sentence and its Σ^ξ₁ negation the results coincide with the complementary bounded statements above, so the impossibility is immediate; but this is the same delicacy Willard had to manage in 2005's footnote 3, where he notes that for a proper Σ\*₁ Υ the string `¬Υ` is not itself syntactically Π\*₁. It is worth stating explicitly in the codified version rather than inheriting as a gloss — I'll add it to the composition obligations when C13 assembles the engine presentation.

---

**User:**

> " so Good is monotone decreasing in N." - what is meant by this?

**Assistant:**

My phrasing there was loose — "monotone decreasing" isn't right for a predicate, which has no magnitude to decrease. What I should have said:

**The set of N at which Υ is Good is downward closed.** If Υ is Good(N), then Υ is Good(M) for every M ≤ N. Equivalently, the map `N ↦ Good_Υ(N)` is *antitone*: as N rises the property gets harder to satisfy, and once it fails it never returns.

**Why.** For Π^ξ₁ Υ = `∀v̄ φ(v̄)`, we have `Scope_E(Υ,N) = ∀v̄ < E(N) φ(v̄)`, and `E(N) = 2^N` grows with N. A larger N therefore quantifies over a *larger* window — it asserts strictly more instances. So if every tuple below `2^N` satisfies φ, then a fortiori every tuple below `2^M` does for `M ≤ N`.

**Why it matters.** This is exactly what makes `♯(Υ)` — "the largest J with Υ Good(J)" — a genuine *threshold* rather than an arbitrary supremum. The good set is an initial segment `{0, 1, …, ♯(Υ)}`, with `♯(Υ) ≈ ⌊log₂ c⌋` for `c` the least counterexample (or the whole of ℕ when Υ is true, giving ∞). Without the downward closure, "largest J" could sit above scattered failures and would not measure anything. With it, ♯ really is *distance to first counterexample, in bits*.

**The Σ side runs the other way**, which is Willard's footnote 6. For Σ^ξ₁ Υ = `∃v̄ φ(v̄)`, `Scope_E(Υ,N) = ∃v̄ < E(N) φ(v̄)`; enlarging the window makes an existential *easier* to satisfy, so the good set is **upward** closed. Hence footnote 6's phrasing: Good(N) is *weaker* than Good(∞) for Π sentences but *stronger* for Σ ones. It also explains why Definition 4.4 defines `♯` only for Π^ξ₁ sentences and Π^ξ₁ sets — for a Σ sentence "the largest such J" is ∞ the moment any witness exists, so the informative threshold there is the *smallest* N, which is why E-stability's conclusion `Good{½⌊Log(p)⌋ − 1}` is asserting that a witness exists *low down*.

**This feeds back into your earlier question 2.** The two polarities being opposite is precisely what makes the collision a contradiction rather than a redundancy. At one and the same level `N = ½♯(Φ)`:

- Υ (Π^ξ₁) Good(N) — *no* counterexample below `2^N`;
- ¬Υ (Σ^ξ₁) Good(N) — a witness *does* exist below `2^N`.

Same window, complementary claims. Had both sentences had the same monotonicity direction, "Good at the same level" would have been consistent and the proof would have had nothing to work with.

---

**User:**

> How does Willard's definition of the threshold for the shortest proof of a contradiction compare to Rosser's Trick?

**Assistant:**

*[Tool calls: counted "Rosser" occurrences across the corpus's `sources-text`; read the mentions in context; checked whether the Rosser-predicate references are engaged in either body.]*

Good question — and it bears directly on the Refinement note in your synthesis document about "deriving the SJAS consistency preservation results from a Rosser's Trick analogue." Let me check what the corpus actually says about Rosser before I compare.

Useful: Rosser appears in Willard only **historically** (the 1936 ω-consistency removal) plus two bibliography entries he never discusses — Arai's *Derivability Conditions on Rosser's Proof Predicates* and Švejdar's *Modal Analysis of Rosser Sentences*.

Confirmed: Arai and Švejdar sit in both bibliographies and are **never engaged in either body**. So the comparison you're contemplating is genuinely open in the corpus — Willard had the Rosser-predicate literature on his desk and didn't connect it. Here is how the two mechanisms actually relate.

#### The two constructions

**Rosser (1936).** Replace Gödel's "I am unprovable" with a *comparative, bounded* sentence:

```
R  ↔  ∀p [ Prf_T(⌜R⌝, p)  →  ∃q ≤ p  Prf_T(⌜¬R⌝, q) ]
```

The move that does the work: `∃q ≤ p` is a **bounded** quantifier, so "is there a shorter refutation?" is a decidable search a weak theory can carry out. Gödel needed ω-consistency because his sentence made an unbounded provability claim; Rosser needs only consistency because his makes a bounded comparison.

**Willard (Definition 4.5, "Tight").** For every inconsistent `β ⊃ B^ξ` of Π^ξ₁ sentences, with `q_β` the shortest proof of `0=1`:

```
Log(q_β)  ≥  ♯(β) + 2
```

and in Theorem 5.9 the operative instance is `Log{Max[p̄,q̄,x̄,ȳ]} = ♯(Φ) + 1` for the minimal counterexample tuple.

#### Where they genuinely agree

Three real parallels, and they are not superficial:

1. **Both convert unbounded self-reference into a size comparison.** Rosser compares two proof lengths; Willard compares a proof length against `♯`.
2. **Both make the decisive step decidable.** Rosser by the bounded search `q ≤ p`; Willard by `Scope_E`, which rebounds every unbounded quantifier to `E(N)` and turns `Good(N)` into a bounded, checkable statement.
3. **Both close on minimality.** Rosser's argument turns on the *given* proof `p` and what lies below it; Willard's on the *least* counterexample tuple. In both, the contradiction is that the minimal object would have to contain something smaller than itself. Willard's 1993 form is almost verbatim Rosser-shaped in this respect: Lemma 6.2 says an inconsistency proof `p` must construct within itself a witness `p*`, and minimality kills it.

#### Where they part — and this is decisive

**The comparison lives in different places.** Rosser's length comparison is *inside the object sentence*: `R` itself talks about shorter proofs. Willard's `SelfCons_k` contains **no length comparison at all** — it says only `∀x∀y∀p∀q ¬{Neg_k(x,y) ∧ SubstPrf(x,p) ∧ SubstPrf(y,q)}`. The threshold lives entirely in the **metatheory**, as a condition (Tightness, or A-/E-stability) that the metatheorist verifies about the configuration. Willard's sentence is a plain consistency assertion; Rosser's is a rigged one.

**And that is deliberate.** Rosser-style rigging is exactly the escape route Willard rules out in the first paragraph of the 1993 chapter: "there exists unnatural deduction methods (which are *numerically correct* but *intensionally incorrect*) that support very strong self-verification [Fe60]. Our research studies a very different type of problem, where the rule of inference is required to be an *intensionally correct*, very natural method." The standard fact behind that remark is that PA **does** prove its own *Rosser*-consistency — because the Rosser predicate is not an honest provability predicate. Willard's whole programme is the refusal of that move: keep "provable" honest, and pay by weakening the arithmetic instead.

So: **Rosser-style self-verification is cheap and uninteresting because it changes what "provable" means; Willard's is expensive and interesting because it changes what "total function" means.** *(This last clause is wrong; see the correction below.)*

**A comparative bound versus a quantitative one.** Rosser's trick needs only that proof lengths be *ordered*; it works in any theory with modest arithmetic. Willard's threshold needs the *absolute magnitudes* to behave — `♯` versus `Log(q)` is a claim about growth rates, which is why it holds only when the language is growth-restricted and fails the moment multiplication becomes total. Rosser is ordinal; Willard is metric.

#### The convergence worth noticing

Both routes evade their respective theorem by breaking **the same derivability condition** — provable closure under modus ponens (HB's second) — for opposite reasons:

- The Rosser predicate fails it *by construction*; Arai's paper, which Willard cites and never discusses, is precisely a study of which derivability conditions Rosser predicates satisfy.
- Willard's systems fail it *by arithmetic weakness*. TR 93-10 p. 12: "only in the *degenerate case* where `x` and `y` are *fixed constants*" can `IS(A)` prove that from a proof of α and a proof of α⊃β some proof of β exists.

That second one is an instance of the pattern I logged as obligation O22 — fixed-parameter totality holding where uniform totality fails. The function that combines two proofs into a third is exactly the sort of growth `IS(A)` cannot prove total.

#### Where Willard's corpus *is* Rosser-flavoured

Not in the positive results — in the negative ones. `Willard2005` §6 uses `f(s,b)`, a Gödel sentence explicitly **restricted by proof length**: "There exists no proof `p` … which satisfies `p < 2^b_s`." Theorem 6 then exhibits a proof `W` short enough that its own existence is precluded by the sentence. That is bounded-provability self-reference in the Rosser idiom, deployed to prove `NS^{k,m}_D(PA+)` *inconsistent*. Willard also cites Verbrugge–Visser's proof-length constraints in 2011 Remark 6.15 as an alternative to his translational reflection — the nearest thing to an acknowledgement of the adjacent literature.

#### For the Refinement stage

Your synthesis document floats "deriving the SJAS consistency preservation results from a Rosser's Trick analogue." On this reading that is a real and unexplored possibility, with one caveat that shapes it: any such derivation must keep the length comparison in the **metatheory**, not smuggle it into the object sentence — otherwise it collapses into the intensionally-incorrect route Willard spent thirty years refusing, and proves nothing about honest provability. The interesting target would be a Rosser-style *metatheorem* about minimal inconsistency proofs that yields Tightness, rather than a Rosser sentence.

One minor erratum surfaced while checking: `Willard2005` §4 Item D cites "Švejdar [55]" for a claim about interpretability, but [55] is *Modal analysis of generalized Rosser sentences* — the interpretability paper is [54]. Probable citation slip.

Want me to record this comparison in the ledgers — as a drift/concordance note plus a Refinement-prep entry alongside the Beklemishev item (G7)? It sits outside the Codification corpus proper, but the O-register is the right home for the caveat about where the comparison must live.

---

**User:**

> First, please record the [55] vs [54] error and the correct reference. Second, Willard does not change what "total function" means (i.e., he does not use an alternative definition of totality).

**Assistant:**

Taking the correction first, because you're right and my formulation was wrong.

**Willard does not redefine totality.** "Total function" means exactly what it always means — `∀x∀y∃z Mult(x,y,z)` is the same sentence for him as for anyone. What changes is the **theory**: which function symbols the language carries and which totality axioms the system takes. He drops multiplication's totality axiom and represents multiplication as a 3-way Δ₀-definable relation; 1993 is explicit that `IS(A)` "will recognize non-zero Division as a total function (and hence can view Multiplication as a well-defined *partial* function)". Nothing is redefined anywhere. The 2011 Type-M/A/S/NS taxonomy is likewise purely a question of *which of axioms (1)–(3) are present*.

So the contrast I was reaching for should have been: **Rosser's route is an intensional change to the provability predicate on a fixed theory — numerically correct, intensionally incorrect. Willard's route redefines nothing and weakens the theory instead.** That is exactly why his is the interesting one: every notion keeps its standard meaning, and the arithmetic simply has fewer axioms.

That misstatement is also a presentational hazard worth guarding against, so let me record both items.

*[Tool calls: verified the Švejdar passage and the two bibliography entries; recorded erratum E6 and obligation O26; logged both; committed as `93b9030`.]*

Both recorded and committed (`93b9030`).

**Erratum E6** — the Švejdar slip, in `willard2005.md` §7. Willard's §4 Item D cites `[55]` twice, in "Hájek, Švejdar and Vopěnka **[55, 63]**" and again in "Švejdar **[55]** has generalized this phenomena with regards to interpretability." But `[55]` is *Modal analysis of generalized Rosser sentences* (JSL 48, 1983), while `[54]` is *Degrees of interpretability* (Comm. Math. Univ. Carolinae 19, 1978) — which is what the claim describes. `[54]` is cited correctly in Item E's list, so he does use it elsewhere. I recorded the reading that both Item D occurrences are slips for `[54]`, but marked it **probable** rather than certain: confirming it would need Švejdar's 1983 paper, which isn't in the corpus, and Švejdar worked on both Rosser sentences and interpretability. The practical consequence is noted — the codified statement should cite that result by title rather than through 2005's numbering.

**Obligation O26** — the totality correction. I've recorded it as a standing constraint rather than just fixing my sentence, because the error I made is the natural one for a reader to make and it would be damaging in the codified statement: it converts an honest weakening of the theory into a definitional trick. The record cites Willard's own phrasing — `IS(A)` "will recognize non-zero Division as a total function (and hence can view Multiplication as a well-defined **partial** function)" — and notes that 2011's Type-M/Almost-M/A/S/NS taxonomy is entirely a question of *which of axioms (1)–(3) are present*, never of what totality means. The corrected contrast is now the recorded one: Rosser alters the provability predicate on a fixed theory; Willard alters the theory and leaves every notion standard.

I also logged the surrounding evidential situation, since it bears on the Refinement thread: Rosser appears in the corpus only historically, and Arai's *Derivability Conditions on Rosser's Proof Predicates* and Švejdar's *Modal Analysis of Rosser Sentences* sit in both spine bibliographies **without being engaged in either body**. So the Rosser comparison your synthesis document contemplates is open rather than foreclosed — and O26 now records the constraint any such comparison has to respect.

Next component when you want it is **C7 — Willard 2001** (JSL 66, 67 pp.), which both 2005 and 2011 delegate their ∆\*₀-encodability appendices to, and which contains the Theorem 7.2 Löb generalization that 2011's reflection result is stated against.

---

*End of transcript. The follow-up instruction — to record this transcript, to
confirm the negation-delicacy obligation, and to proceed to C7 — is logged in
`../../LOG.md` under the C7 entry.*

---

## Addendum 2026-08-26 — correction arising from C7

The transcript above is left verbatim. One claim in it is wrong, and the
correction is recorded here rather than by editing the record.

**What was said.** In the Rosser comparison I stated that Willard's threshold
on the shortest proof of a contradiction "lives entirely in the metatheory",
in contrast to Rosser's trick, which builds its size comparison into the
provability predicate itself.

**What is actually the case.** That is true of `Willard2005`, of
`Willard2011`, and of `Willard2001`'s `IS(A)` — whose Group-3 axiom is the
flat `∀y ¬SemPrf_{IS(A)}(⌜0=1⌝, y)` (Eq. 8), with all bounding done in the
analysis. It is **false** of `Willard2001`'s schematic systems, where the size
comparison is an explicit clause of the object-language axiom:

| System | Group-3 clause |
| --- | --- |
| `IS^λ(A)` | `y^λ < z/x` (Eq. 9) |
| `ISREF(A)` | `Size(y) ≤ x − 1` (Eq. 10) |
| `ISTR(A)` | `Size(y) < x ≤ z/2` (Eq. 16) |
| `ISM(A)` | `y·Log₂(x+2) < Log₂ z` (Eq. 61) |

So the corpus contains **both** designs, and 2001 contains both itself. The
distinction is recorded as drift **D26** and becomes a design axis at C13:
Level-0 systems put the bound entirely in the metatheory, Level-1 systems
quantify over it inside the axiom.

**What survives.** The substance of the Rosser contrast is unaffected, and is
worth restating in its corrected form: *Rosser alters the provability predicate
of a fixed theory, leaving the theory's axioms alone; Willard alters the theory
— adding a Group-3 axiom, sometimes one carrying a size clause — and leaves
the provability predicate, and every other notion, standard.* The point that
obligation **O26** protects (Willard redefines nothing) is strengthened rather
than weakened by this correction: even where a size comparison appears inside
a Willard axiom, it is an ordinary arithmetical clause of an ordinary axiom,
not a reinterpretation of what "provable" means.

The phrase "lives entirely in the metatheory" must not be carried into the
codified statement.
