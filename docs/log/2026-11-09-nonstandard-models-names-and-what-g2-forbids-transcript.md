# Transcript: nonstandard models, names and denotations, and what G2 and Löb forbid (2026-11-08 to 2026-11-09)

Verbatim turns from the Claude Code session, extracted by script. Omitted: lane checks. Summary and status: [note](2026-11-09-nonstandard-models-names-and-what-g2-forbids.md).

### 2026-11-08 19:44 UTC — User

What are other examples of non-standard models? That is, do domains other than N have non-standard models?

### 2026-11-08 19:45 UTC — Claude

Yes. Every infinite structure has nonstandard models, and no finite structure does.

The reason is first-order logic, not anything special about ℕ. Take an infinite structure M and add a constant c, along with infinitely many axioms saying "c differs from each element you can name." By compactness, the full first-order theory of M plus these axioms has a model, and that model is not M. The Löwenheim–Skolem theorems add that the theory has models of every infinite cardinality. A finite structure, by contrast, is pinned down up to isomorphism by its theory.

What differs from domain to domain is what the extra models look like.

**The reals.**
- **Hyperreals.** Robinson's nonstandard analysis uses \*ℝ, an ultrapower of ℝ. It satisfies every first-order truth about ℝ, and it adds infinitesimals and infinite numbers. Unlike PA's nonstandard models, which only add elements beyond the standard ones, \*ℝ puts new elements between the standard reals: each real has a halo of infinitely close neighbours.
- **Real closed fields.** The theory of (ℝ, +, ·, <) is the theory of real closed fields (Tarski). Its models include the real algebraic numbers, which are countable and smaller than ℝ, and the real Puiseux series, where t is a positive infinitesimal. So a nonstandard model can be smaller than the intended one, not only larger.
- **Inside the hyperreals.** \*ℕ is a nonstandard model of all true arithmetic, not just PA.

**The complex numbers.** The theory of algebraically closed fields of characteristic 0 has exactly one model of each uncountable size (Steinitz), so every such field the size of the continuum is isomorphic to ℂ. Nonstandard models appear only at countable size: the algebraic closures of ℚ(t₁, …, tₙ), one for each transcendence degree n ≤ ω.

**The rationals as an order.** Dense linear orders without endpoints have only one countable model, ℚ (Cantor). Nonstandard models appear only at uncountable sizes, for example ℝ, or ℝ followed by ℚ. The random graph behaves the same way.

These two cases show the general pattern. Morley's theorem says a theory with one model in one uncountable size has one model in every uncountable size. Which sizes have a unique model differs from theory to theory. PA has non-isomorphic models in every infinite size, which is the worst case.

**Presburger arithmetic (ℕ, +).** This one is instructive because Tennenbaum's theorem fails here. Take the elements n + q·t, where n is an integer and q is a positive rational, together with the ordinary naturals. Order them first by q, then by n, and add them componentwise. This is a computable nonstandard model. Tennenbaum's theorem needs both addition and multiplication; with addition alone, a nonstandard model can be computable.

**Set theory.** If ZFC is consistent, then:
- It has a countable model (downward Löwenheim–Skolem). This is Skolem's paradox: the model proves "uncountable sets exist" while being countable from outside.
- It has models whose membership relation is not well-founded.
- It has models whose ω is nonstandard. A model of ZFC + ¬Con(ZFC) is one of these: its "proof of 0=1" is a nonstandard number, the same phenomenon as PA + ¬Con(PA) from our earlier discussion. So set theory has nonstandard natural numbers inside it.

**Second-order theories.** Second-order arithmetic and the theory of complete ordered fields have only one model each (Dedekind), but only under full semantics, where set quantifiers range over every subset. Under Henkin semantics, where sets range over a chosen collection, nonstandard models return. Reverse mathematics uses these models routinely; for example, the recursive sets form the smallest ω-model of RCA₀.

**Why this matters for the project.** Inside Lean or Ansatz, `Nat` is an inductive type, and the theory itself proves that any two structures meeting its specification are isomorphic. But that categoricity holds only inside the type theory. A model of the type theory built in an ω-nonstandard universe of set theory interprets `Nat` as that universe's ω, which is nonstandard when viewed from outside. That is the precise sense in which our results, such as D1 and eventually G2 for `conPA`, hold only relative to Lean/Ansatz.

The S1 scope question from my last report is still open, and p2 stays paused until you answer it.

### 2026-11-08 19:52 UTC — User

What are two examples of different infinite cardinalities?

### 2026-11-08 19:52 UTC — Claude

The standard pair is the natural numbers ℕ and the real numbers ℝ.

**ℕ is countable.** Its cardinality is ℵ₀. Any set that can be listed as a first, second, third element and so on has this size. That includes the integers, the rationals, the real algebraic numbers, and all finite strings over a finite alphabet, so it also includes every PA formula and every proof.

**ℝ is uncountable.** Its cardinality is 2^ℵ₀, the continuum. Cantor's diagonal argument proves it is larger. Suppose someone gives a list r₀, r₁, r₂, … of reals. Build a new real whose n-th decimal digit differs from the n-th digit of rₙ (avoiding 0s and 9s, so that no number has two decimal expansions). The new real differs from every rₙ at some digit, so it is not on the list. Hence no list contains every real.

**The hierarchy does not stop at two.** Cantor's theorem says the set of all subsets of any set S is strictly larger than S. So ℕ, ℝ, the set of all subsets of ℝ, the subsets of that, and so on form an endless chain of ever-larger infinite cardinalities. Whether any cardinality lies strictly between ℕ and ℝ is the continuum hypothesis, which ZFC can neither prove nor refute (Gödel, Cohen).

These two sizes explain the earlier examples:
- The real algebraic numbers and ℝ can both be models of the theory of real closed fields, yet they cannot be isomorphic, because one is countable and the other is not.
- ℚ and ℝ are both dense linear orders without endpoints, and they cannot be isomorphic for the same reason.

### 2026-11-08 19:56 UTC — User

So PA has aleph-100 sized models, not isomorphic to N?

### 2026-11-08 19:57 UTC — Claude

Yes. In fact PA has models of every infinite cardinality, and none of the uncountable ones can be isomorphic to ℕ, simply because ℕ is countable.

**Why they exist (a theorem of ZFC).**
1. Add ℵ₁₀₀ new constants c_α, with axioms saying any two of them are distinct.
2. Any finite set of these axioms, together with PA, holds in ℕ: interpret the finitely many constants mentioned as distinct numbers.
3. By compactness, the whole theory has a model, and that model has at least ℵ₁₀₀ elements.
4. By downward Löwenheim–Skolem, there is a model of exactly ℵ₁₀₀ elements. Start from all of ℕ's true sentences instead of PA, and you even get a model of true arithmetic.

**What such a model looks like.**
- **Ordering.** ℕ sits at the bottom as an initial segment. Above it are blocks of nonstandard numbers, and each block is a copy of the integers (…, a−1, a, a+1, …). The blocks are themselves ordered densely, with no first or last block, and there are ℵ₁₀₀ of them. In short, the order type is ω + ζ·A, where A is a dense order of size ℵ₁₀₀.
- **What it satisfies.** It satisfies every axiom of PA. If you start from true arithmetic, it satisfies exactly the same first-order sentences as ℕ, so no sentence can tell the two apart.

**How many there are.** At size ℵ₁₀₀ there are 2^ℵ₁₀₀ pairwise non-isomorphic models of PA, the largest number possible. This follows from Shelah's classification theory: PA defines an infinite linear order, which makes it "unstable", and unstable theories have the maximum number of models in every uncountable cardinality. Even at the countable size there are 2^ℵ₀ different models.

**In this project's terms.** Statements like this are theorems of ZFC. ZFC builds ℕ as a model of PA, and everything above is constructed from that. So "PA has models of size ℵ₁₀₀" carries the same relativity caveat as our Lean/Ansatz results: it is true in whatever universe of sets the reasoning takes place in.

### 2026-11-08 20:00 UTC — User

Why can PA not rule out non-standard naturals?

### 2026-11-08 20:01 UTC — Claude

PA can't rule them out because no first-order sentence can say "x is a standard number." The induction schema comes closest, but it only covers properties that formulas can define, and the set of standard numbers is never one of them.

**The direct reason: compactness.** Add a new constant c to PA, along with the infinitely many axioms c > 0, c > 1, c > 2, and so on.
- Any finite set of these axioms holds in ℕ: interpret c as a number larger than the finitely many it is compared with.
- By compactness, the whole set has a model. In that model c is larger than every standard number, yet every theorem of PA still holds.

The same argument works for any set of first-order axioms true in ℕ, including all of true arithmetic. So no first-order axioms can do it, not just PA's. The obvious attempt, "x = 0 ∨ x = 1 ∨ x = 2 ∨ …", is an infinite disjunction, and formulas are finite.

**Why induction does not do it.**
- **Second-order induction would.** Peano's original axiom (Dedekind's version) says: every set that contains 0 and is closed under successor contains every number. In a nonstandard model, the standard numbers form such a set, but they are not everything, so the axiom would be violated. That is why second-order PA has ℕ as its only model.
- **First-order PA has only a schema.** It has one induction axiom per formula φ(x), so only countably many sets are covered. ℕ has 2^ℵ₀ subsets, and the set of standard numbers is never among the ones covered.
- **This is provable, not an accident.** Suppose some formula φ, possibly with parameters, defined the standard numbers in a nonstandard model. Then φ(0) holds, and φ(n) implies φ(n+1). Induction then gives φ for every element, contradicting the existence of a nonstandard element. So nonstandard models satisfy induction precisely because their standard part is invisible to formulas.
- **Overspill.** A consequence of the same fact: any formula true of every standard number is also true of some nonstandard one.

**The underlying trade-off.** Proofs are finite, so any contradiction uses only finitely many axioms. That makes compactness a consequence of any proof system that is sound and complete for its semantics. Compactness in turn produces nonstandard models.
- Second-order logic escapes by giving up a complete proof system: no effective proof system captures all its truths.
- Lindström's theorem makes this exact: first-order logic is the strongest logic that has both compactness and the downward Löwenheim–Skolem property.

So you can have an effective, complete proof system, or a theory whose only model is ℕ, but not both.

**A second, separate gap.** Compactness gives nonstandard models that satisfy exactly the same sentences as ℕ. Gödel's first incompleteness theorem gives models that disagree with ℕ about particular sentences. For example, a model of PA + ¬Con(PA) contains a nonstandard "proof" of 0 = 1. Only the second kind matters for what PA can prove.

**In this project's terms.** Our in-house PA (`PAx`'s `ind`) is the first-order schema, so it has this limitation. Ansatz's `Nat` gets induction from its recursor, which covers every predicate, much like second-order induction. So `Nat` has only one model inside the type theory. But Ansatz's own proof system is effective, so externally the type theory has models whose `Nat` is nonstandard. This is the same relativity caveat as before.

### 2026-11-08 20:06 UTC — User

What is compactness? The distinction between an axiom schema and a finite second-order quantification over sets seems transparently to be an effect of different resource consumption requirements between naming and using (sets of) formulae.

### 2026-11-08 20:06 UTC — Claude

**Compactness** says: a set of first-order sentences has a model if and only if every finite subset of it has a model.

The usual route to it goes through proofs:
- Proofs are finite, so a proof of a contradiction uses only finitely many axioms.
- By the completeness theorem, a set of sentences has a model exactly when no contradiction can be proved from it.
- So if a set has no model, some finite subset already proves a contradiction, and that subset has no model either.

Compactness can also be proved purely semantically, with ultraproducts, but its content is the same: no first-order consequence can depend on infinitely many axioms at once. In the earlier argument, each finite piece of "c > 0, c > 1, c > 2, …" holds in ℕ. So the whole set has a model, and in that model c is nonstandard.

**On your observation.** I think it is right about proofs. Where categoricity comes from needs one refinement.

**1. In proofs, second-order quantification turns back into naming.**
- The axiom ∀X (0 ∈ X ∧ ∀x (x ∈ X → Sx ∈ X) → ∀x (x ∈ X)) is a single finite sentence.
- To use it on a property φ, you must know the set {x : φ(x)} exists. That takes an instance of the comprehension schema, ∃X ∀x (x ∈ X ↔ φ(x)).
- So every application of second-order induction in a proof goes through a named formula, just as the first-order schema does. Each proof can use only finitely many of those names.
- Second-order arithmetic proves more than PA, because its formulas can themselves quantify over sets. But the mechanism is the same: induction on whatever the proof can name.

**2. Categoricity comes from the semantics, not the axiom.**
- Full second-order semantics stipulates that X ranges over all subsets of the domain, as the ambient set theory supplies them.
- If you instead treat sets as a second sort of ordinary objects (Henkin semantics), the same axiom and comprehension schema regain compactness and nonstandard models.
- So the sentence is not doing the work. The stipulation about its range is.

That stipulation consumes no resources because nothing ever checks it. The price shows up elsewhere: the valid sentences of full second-order logic are not recursively enumerable, so no proof system can capture its consequences. Categoricity is bought by a semantics that outruns every finite naming process.

**My refinement of your formulation:**
- The schema quantifies over names; full second-order quantification quantifies over denotations.
- A finitary process, whether a proof or a checker, can only ever use names.
- Nonstandard models live in the gap between the sets that can be named and all the sets those names could be drawn from.
- Compactness is the semantic statement that this gap can never be closed from the naming side. Lindström's theorem says no logic with compactness and downward Löwenheim–Skolem can be stronger than first-order logic.

**In Ansatz,** `Nat.rec`'s motive ranges over all predicates, which looks like second-order induction. But every use instantiates the motive with a term, which is a name. The internal proof that any two `Nat`-like structures are isomorphic works only because both structures are named in the same theory. Externally, the type theory's models still include nonstandard `Nat`s, which is your point carried over to the system we are building on.

### 2026-11-09 04:41 UTC — User

What do you mean by "names" vs "denotations"?

### 2026-11-09 04:41 UTC — Claude

A **name** is a piece of syntax: a formula φ(x), a finite string of symbols. Its **denotation** in a model M is the set of elements it picks out there, {a ∈ M : M ⊨ φ(a)}.

**The same name can denote different sets in different models.** The formula "∃y (x = y + y)" denotes the even numbers. In ℕ that is {0, 2, 4, …}. In a nonstandard model it also contains nonstandard even numbers. The string stays fixed; the model decides what it covers.

**The two forms of induction quantify over different things.**
- **The schema** says: *for every formula φ*, the induction axiom for φ holds. That "every" lives in the metalanguage and ranges over strings. There are only countably many, and each instance then speaks about whatever set φ denotes in the model at hand.
- **The second-order axiom** says ∀X (…). That "every" lives in the object language and, under full semantics, ranges over all subsets of the domain, whether or not any formula denotes them.

**Most sets have no name.** ℕ has 2^ℵ₀ subsets but only countably many formulas, so almost every subset is undefinable. Two concrete examples:
- **Truth.** The set of codes of true arithmetic sentences has no name in the language of arithmetic. That is Tarski's theorem on the undefinability of truth.
- **The standard part.** In a nonstandard model M, the set ℕ ⊂ M has no name, even with parameters. Induction on any formula naming it would make it all of M.

That second example is the earlier argument, now stated precisely. The schema covers every set a formula denotes. The second-order axiom also covers sets no formula denotes, and the standard part of a nonstandard model is one of those.

**How this connects to your resource point.**
- A proof is finite, so it can contain names but never denotations, since a denotation is typically an infinite set.
- A proof can therefore only ever "use" a set by writing down a formula that denotes it.
- Full second-order semantics quantifies over sets that no finite object can point to. That is why no proof system captures it.

**In the formalization,** the distinction is explicit:
- **Names.** `PF` terms such as `prForm D` are names: finite syntax trees.
- **Denotations.** `paHolds ρ f` computes a name's denotation in the standard model ℕ, at the assignment ρ.
- **Crossing back.** W7's `s2_concrete` goes from denotation to name. If a Σ₁ formula's denotation contains the given numbers (`paHolds` is true), then PA proves the formula with those numbers substituted as numerals. That direction works for Σ₁ formulas, because their truth is witnessed by finite data. For arbitrary formulas it fails, and that failure is incompleteness.

### 2026-11-09 07:42 UTC — User

What is meant by "full" semantics?

### 2026-11-09 07:42 UTC — Claude

"Full" semantics, also called standard semantics, is a way of interpreting second-order logic. A set variable X ranges over every subset of the domain, and a relation variable ranges over every relation on it. The alternative is Henkin semantics.

**In full semantics, a structure is just a domain** D, together with its constants and operations. The range of ∀X is not part of the structure. It is fixed automatically as the power set 𝒫(D).

**In Henkin semantics** (or "general" semantics), **a structure has an extra component**: a chosen collection S ⊆ 𝒫(D), and ∀X ranges only over S.
- S is normally required to satisfy the comprehension axioms, so that every definable set is in it.
- Beyond that, S may leave sets out. For example, S could be just the definable sets, or just the computable ones.
- Second-order logic under Henkin semantics is really many-sorted first-order logic, with sets as a second sort of object. So it regains a complete proof system, compactness and Löwenheim–Skolem, and with them nonstandard models.

**How the two compare:**

| | Full | Henkin |
|---|---|---|
| Range of ∀X | all of 𝒫(D) | a chosen S ⊆ 𝒫(D) |
| Second-order PA | categorical: only ℕ | has nonstandard models |
| Complete effective proof system | no (validity is not recursively enumerable) | yes |
| Compactness | fails | holds |

In terms of names and denotations: a Henkin structure decides which denotations exist, while full semantics says all of them do.

**"All subsets" is decided by set theory, not by logic.**
- What 𝒫(D) contains is determined by whatever set theory the semantics is read in.
- Full semantics can therefore make a second-order sentence's truth depend on set theory. For instance, there is a second-order sentence that is valid in full semantics exactly when the continuum hypothesis holds.
- ZFC is itself a first-order theory with nonstandard models, including countable ones (Skolem's paradox). Read inside one of those, "all subsets of ℕ" means only that model's subsets.
- So the categoricity of second-order PA is categoricity relative to a universe of sets. It does not escape the first-order limits; it moves them into the metatheory. This was Skolem's objection, and it is the same caveat as Ansatz's internal categoricity.

**For Ansatz and Lean:**
- The intended set-theoretic model of the type theory gives `Nat → Prop` the full semantics: all subsets of ℕ.
- The proof system is effective, so it cannot be complete for full models. It is complete only for general, Henkin-style models (Henkin proved this for simple type theory). Some of those models, such as realizability models, include only some subsets.
- So when Ansatz proves something about all predicates on `Nat`, the proof holds in every general model, not only the full one. Each step uses only predicates that a term names.

### 2026-11-09 08:24 UTC — User

What prevents a first order theory from being a theory only for an intended model which is that theory itself under Herbrand semantics, that is, a model in which the syntax of the theory generates the elements of the model (variables become constants)?

### 2026-11-09 08:25 UTC — Claude

Nothing prevents it as a semantics; what it gives up is a finite proof system. The term model exists, and for PA it is ℕ. But if only such models count, the theory's consequences become all of true arithmetic, which no finite proof system can generate. The idea also splits in two depending on which terms you allow, and only the stricter version excludes nonstandard elements.

**PA's term model is ℕ.** Take the closed terms built from 0, S, + and ·, and identify two of them when PA proves them equal.
- Even Robinson's Q proves every closed term equal to a numeral, for example S0 + S0 = SS0.
- Distinct numerals are provably distinct.
- So the classes of closed terms correspond exactly to the numerals: the term model is ℕ.

Henkin's proof of the completeness theorem builds models in this way, from syntax.

**Restricting to term models makes ∀ substitutional.** Under "Herbrand semantics", ∀x φ(x) is true exactly when φ(t) is true for every closed term t. For PA that means φ(0̄), φ(1̄), φ(2̄), … That is the ω-rule: from infinitely many premises φ(n̄), infer ∀x φ(x).
- **The only model is ℕ.** PA's term model is ℕ, so ℕ is the only model under this semantics.
- **Its consequences are true arithmetic.** That set is not recursively enumerable (Gödel, Tarski), so no finitary proof system is complete for it.
- **Concretely,** the Gödel sentence G = ∀x ¬Prf(x, ⌜G⌝) is a consequence under substitutional semantics. Provided PA is consistent, each instance ¬Prf(n̄, ⌜G⌝) is a true, decidable sentence that PA proves. G itself is not provable in PA.
- **Compactness fails as well.**

So the semantics is fine; the cost is a proof rule with infinitely many premises. In your terms, a single "name" for a proof of ∀x φ(x) would have to contain infinitely many names. Gentzen-style consistency proofs use exactly this system, PA with the ω-rule: finite PA proofs embed into its infinitary proofs, and ε₀ measures how deep they reach.

**Allowing all terms brings nonstandard elements back as names.** Herbrand's theorem applies only to universal theories. PA's axioms contain ∃, so to put PA in Herbrand form you Skolemize: each ∃ gets a new function symbol that produces a witness. The terms built from these symbols enlarge the domain, and they need not equal any numeral.
- Take PA + ¬Con(PA), which is consistent if PA is (G2).
- Skolemizing ∃p Prf(p, ⌜0=1⌝) gives a constant w.
- PA proves w ≠ n̄ for every n, since each standard n is decidably not a proof of 0=1.
- So w names a nonstandard element. Every element of this term model has a name, and some of them are nonstandard.

This sharpens the earlier "names vs denotations" point:
- **Individual nonstandard elements can have names.** A Skolem or Henkin constant can denote one.
- **The set of standard elements has no defining formula.** That is the earlier undefinability result.
- **"Every element is a numeral"** is exactly the infinite disjunction "x = 0 ∨ x = S0 ∨ …". No finite formula expresses it, and imposing it from outside is the ω-rule again.

**This is logic programming's semantics.** A Prolog program's intended model is its least Herbrand model, and this is where the trade-off shows up in practice:
- **Existential queries are complete.** SLD resolution finds every true ∃-query, by producing a term witness. That is the Σ₁ case, the same fact as W7's `s2_concrete`.
- **Universal queries are not.** ∀x p(x) over the Herbrand universe needs either an induction proof, which is finite but incomplete, or the ω-rule, which is complete but infinite. Negation as failure and the closed-world assumption are the practical compromise, and they are sound only in restricted cases.

### 2026-11-09 23:52 UTC — User

Ultimately, we are interested in the limits of what a self-reflective, self-modifying formal system can know and guarantee of itself, by itself. Self-justification, and especially self-justification with tangible reflection, or other internal soundness guarantees, seems promising as an exploration of to what degree this, in fullest generality impossible, capability can be realized. To understand this, we must understand what self-justification "really means", which requires understanding what G2 and Loeb's Theorem "really mean", for which our current standard is "a computational interpretation", and best guess is "something about necessary resource consumption when a meta-interpreter statically analyzes its own programs". Connecting this to the in/ability of a theory to rule out non-standard models may be a useful route; regardless, the goal remains unmet of "really understanding" SJAS, and how it might support an autarkic system.

### 2026-11-09 23:56 UTC — Claude

Your guess and the nonstandard-model route turn out to be the same thing. I can now state it more sharply than "resource consumption".

**Thesis.** A system can certify, by a single finite proof, that every one of its proofs in some class is sound only if it can write down the invariant that makes all those proofs sound at once. What a nonstandard model exploits is exactly the cases where that invariant would have to be infinite.

**The two resources.** Suppose a model of PA contains a nonstandard proof of ⊥. If every formula in it had complexity at most some standard k, the partial truth predicate for complexity k would be an ordinary formula. Induction along the proof's lines (also available in the model) would then show every line is true, so ⊥ could not be the conclusion. So any nonstandard refutation must contain formulas of nonstandard complexity. The obstruction has two components:

- **Invariant size: what the proof's formulas say.** Soundness for proofs whose formulas have complexity ≤ k needs a truth predicate of complexity about k. For all k at once it needs a full truth predicate, which Tarski rules out. In the earlier vocabulary, it is a denotation with no name. For PA this is the only component, because induction already handles the length of proofs.
- **Value growth: how large the numbers they mention get.** For weak theories, the numbers a proof mentions may exceed what the theory proves total. This is where definable cuts come in, as in your 2026-10-29 note.

Your 2026-10-29 note's §3 already shows both components at work. Pudlák's cut-localized consistency for cut-free deduction works because the subformula property fixes the invariant and the cut bounds the values. The thesis above makes those two the general account.

**What G2 and Löb mean in this reading.**

- **G2 forbids paying in advance, never paying as you go.**
    - For each standard n, PA proves ¬Prf(n̄, ⌜⊥⌝), provided PA is consistent. So Con holds instance by instance. What fails is collapsing the ω-rule into one finite proof, which is the Herbrand point from before.
    - Pudlák (1986, as I recall it, not yet checked against the paper) measures the price. A finitely axiomatized sequential theory proves "no proof of ⊥ of length ≤ n" by proofs of size polynomial in n, and needs at least n^ε. The polynomial is the cost of a truth predicate for formulas of size ≤ n.
    - So self-trust up to a horizon is cheap. Self-trust without a horizon has no price at all, because no finite object can pay it.
- **Löb's theorem is what the cost of quotation (D3) forbids.**
    - Bounded provability □ₙφ ("φ has a proof of size ≤ n") satisfies bounded reflection, □ₙφ → φ, by that same partial-truth construction (my derivation, not a cited result). Yet Löb's theorem fails for □ₙ, since otherwise every φ would be provable.
    - What breaks is D3: quoting a proof of size n produces a proof of size f(n) > n. So □ₙφ → □_{f(n)} □ₙφ, the horizon rises with each quotation, and the diagonal never closes.
    - With unbounded □, quotation is free, the diagonal closes, and uniform trust collapses to "□φ → φ only when φ is already proved".
    - This is your 2026-10-29 guarded-trust theorem from the other side. There, "Löb's clause fails its bound"; here, D3's cost is that bound.
- **Induction and self-trust are the same resource.** Kreisel and Lévy showed that PA is equivalent to a weak base, such as PRA, plus full uniform reflection for that base (recalled, not checked). So the schema that tries, and fails, to exclude nonstandard numbers is exactly the trust a theory places in a weaker system's proofs. A theory spends its induction trusting what lies below it. Trusting itself would need the next level up, which is the Turing–Feferman progression.

**Where SJAS sit.**
- Willard's systems neither pay in advance nor pay as you go. They take the guarantee on credit: consistency for the deduction method D is an axiom.
- Choosing a cut-free D fixes the invariant. Not proving multiplication total bounds growth, so the diagonal can never call in the credit.
- That the credit is sound is established outside the system, relative to the metatheory. That is the precise sense in which an SJAS is not autarkic about its own foundation, even though it states its own consistency.

**What this implies for an autarkic system.**
- A running machine only ever holds standard proofs. Nonstandard models are the gap between what a theory must guarantee in all its models and what a machine needs for the proofs it actually encounters.
- Everything the machine needs is in the affordable part: Con for size ≤ n, and reflection for proofs of size ≤ n, both at polynomial cost. This is tangible reflection, and your trust-horizon note's "testing up to J as a certificate of horizon J" is its concrete form.
- A self-modifying system can therefore re-certify itself after each change, up to the horizon it can afford.
- What it can never certify is that paying as you go will always succeed. That statement is G2 itself, and the models where it fails are the nonstandard ones.
- So the autarkic floor is: everything checkable is checked internally, and the system's own standardness — that it really runs finite computations — is the one fact it takes from outside.

**Bearing on the pending S1 decision.** D3 is internal Σ₁-completeness: the in-house PA reproducing W7's `s2_concrete` about itself. So the infrastructure decision I put to you after D1 is also an experiment: it would measure f, the price of quotation, in our own artifact. That does not change the scope question; the paper needs only the statement. But if the aim is understanding, a formal D3 with its proof-size cost is direct evidence for the thesis.

The thesis is my synthesis. The Pudlák and Kreisel–Lévy results are recalled, and the bounded-Löb argument is my own derivation, so all three need checking against sources. I can record this as a docs/log note linking the 2026-10-29 and 2026-11-05 notes, with those checks marked open. A natural formal test would add to the trust-horizon toy a proof that Löb fails for □ₙ, with the size of D3's quotation computed explicitly.

