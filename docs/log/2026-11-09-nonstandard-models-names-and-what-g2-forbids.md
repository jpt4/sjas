# Nonstandard models, names and denotations, and what G2 and Löb forbid

Dates: 2026-11-08 to 2026-11-09

Full record: [transcript](2026-11-09-nonstandard-models-names-and-what-g2-forbids-transcript.md),
with every request and answer verbatim. This note states what the answers
established and what is unchecked.
Related: the [definable-cuts note](2026-10-29-nonstandard-numbers-and-definable-cuts.md)
and the [trust-horizon note](2026-11-05-trust-horizon.md). The discussion continues
in the [2026-11-12 note](2026-11-12-artemov-willard-selectors-and-lambda-cert.md).

## Requests (in order; verbatim in the transcript)

1. Do domains other than ℕ have nonstandard models? What are two different infinite
   cardinalities? Does PA have models of size ℵ₁₀₀?
2. Why can PA not rule out nonstandard naturals?
3. What is compactness? With the user's observation: the difference between an axiom
   schema and a second-order quantifier looks like a difference in the resources
   needed to name and to use sets of formulas.
4. What do "names" and "denotations" mean? What is "full" semantics?
5. What prevents a first-order theory from having, as its only intended model, the
   theory itself under Herbrand semantics?
6. The aim: the limits of what a self-reflective, self-modifying formal system can
   know and guarantee of itself, by itself; what self-justification, G2 and Löb's
   theorem mean computationally; whether the nonstandard-model route helps.

## 1. Nonstandard models in general

- Every infinite structure has them and no finite one does. Compactness gives a
  model with a new element; Löwenheim–Skolem gives models of every infinite
  cardinality.
- Their shape varies. The hyperreals put new elements between the standard ones. The
  real algebraic numbers are a model of the theory of the real field smaller than ℝ.
  Algebraically closed fields of characteristic 0 and dense linear orders have one
  model in some cardinalities (Steinitz, Cantor, Morley). Presburger arithmetic has a
  computable nonstandard model, so Tennenbaum's theorem needs both + and ·.
- PA has models of every infinite cardinality, with the largest possible number of
  non-isomorphic ones: 2^ℵ₀ countable models, and 2^ℵ₁₀₀ of size ℵ₁₀₀.
- ZFC, if consistent, has models whose ω is nonstandard. Second-order arithmetic has
  one model only under full semantics.
- For the project: `Nat` has one model inside the type theory. Externally the type
  theory has models whose `Nat` is nonstandard. That is the sense in which the
  formal results are relative to Lean and Ansatz.

## 2. Why PA cannot exclude nonstandard numbers

- No first-order formula says "x is standard". If a formula defined the standard
  part of a nonstandard model, induction on that formula would make it the whole
  model.
- The schema has one induction axiom per formula. The second-order axiom covers
  every subset.
- Proofs are finite, so a complete proof system gives compactness, and compactness
  gives nonstandard models. An effective complete proof system and a single model
  exclude each other (Lindström's theorem states the exact trade).
- There are two kinds of nonstandard model: those that satisfy the same sentences as
  ℕ (compactness), and those that disagree with ℕ (G1). Only the second kind bears
  on what PA proves.

## 3. Names and denotations

- A name is a formula. Its denotation in a model is the set it picks out there.
- The schema quantifies over names, in the metalanguage. Full second-order
  quantification ranges over denotations, named or not.
- In a proof, the second-order axiom is used only through comprehension for a named
  formula, and a proof uses finitely many names. So the user's observation is right
  about proofs.
- The single model comes from the stipulation of full semantics, which no finite
  process checks. Its price is that validity is not recursively enumerable. Under
  Henkin semantics the same axioms have nonstandard models.
- "All subsets" is fixed by the ambient set theory, so second-order categoricity is
  relative to a universe of sets (Skolem's objection).
- Two sets with no name: arithmetic truth (Tarski), and the standard part of a
  nonstandard model.
- In the formalization: `PF` terms are names; `paHolds` computes a name's denotation
  in ℕ; W7's `s2_concrete` goes from denotation back to name for Σ₁ formulas.

## 4. Herbrand semantics

- Nothing prevents it as a semantics. PA's term model over 0, S, + and · is ℕ.
- Admitting only that model makes ∀ substitutional, which is the ω-rule. The
  consequences are then true arithmetic, which no finitary proof system generates,
  and compactness fails.
- With Skolem terms the domain grows. In PA + ¬Con(PA), the Skolem constant for the
  refutation names a nonstandard element. So single nonstandard elements can have
  names; the set of standard elements has no defining formula.
- Logic programming uses the least Herbrand model. Existential queries are complete
  (the Σ₁ case). Universal queries need induction or the ω-rule.

## 5. The synthesis (answer of 2026-11-09 23:56 UTC)

- **Thesis (mine).** A system can certify by one finite proof that all its proofs in
  a class are sound only if it can write down the invariant that makes them sound at
  once. Nonstandard models exploit the cases where that invariant would be infinite.
- **Two resources.**
  - *Invariant size.* Soundness for formulas of complexity ≤ k needs a truth
    predicate of complexity about k. For all k at once none exists (Tarski).
  - *Value growth.* The numbers a proof mentions may exceed what the theory proves
    total. This is where definable cuts enter.
- **G2.** For each n, PA proves that n is not a refutation, if PA is consistent. G2
  forbids one finite proof covering all n. Bounded consistency is cheap (Pudlák
  1986, recalled).
- **Löb.** As stated then: Löb's theorem fails for bounded provability □ₙ, because
  quoting a proof of size n gives one of size f(n) > n. Corrected on 11-11: that
  holds at a fixed n, or with budgets paid at full size. Critch's Theorem 3 gives
  Löb's theorem for the parametric bounded case.
- **Induction and self-trust.** PA is a weak base plus uniform reflection for that
  base (Kreisel–Lévy, recalled).
- **Willard's systems.** Consistency for the deduction method is an axiom. A
  cut-free method fixes the invariant, and not proving multiplication total bounds
  growth. That the axiom is sound is established outside, relative to the
  metatheory.
- **For an autarkic system.** A running machine holds only standard proofs.
  Consistency and reflection for proofs of size ≤ n are affordable, so the system
  can re-certify itself after each change, up to the horizon it can pay for. It
  cannot certify that this will always succeed; that statement is G2. Its own
  standardness is the one fact it takes from outside.
- **S1.** D3 is internal Σ₁-completeness. Formalizing it would measure the price of
  quotation in the project's own artifact.

## Status

- §§1–4 are textbook results stated from memory. No source was opened for them.
- §5's thesis and the bounded-Löb argument are mine and not machine-checked.
- Pudlák 1986 and Kreisel–Lévy were recalled, not read.
- Offered and not done: a check in the trust-horizon toy that Löb fails for □ₙ, with
  the size of the quotation computed.
