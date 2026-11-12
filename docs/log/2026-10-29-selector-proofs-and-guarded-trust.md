# Artemov's selector proofs and guarded trust

Date: 2026-10-29

Follows the [revised essence note](2026-10-29-essence-of-self-justification-after-lob.md).
Builds on the [2026-10-06 note](2026-10-06-artemov-lp-and-lambda-cert.md),
whose §2 item 4 and §4 already compare Artemov with λᶜᵉʳᵗ₀.

## Request (verbatim)

> How does this compare to Artemov's Selector Proofs?

## Sources, with page-image checks

- S. Artemov, "The provability of consistency", arXiv:1902.07404v5. Held at
  `nachlass/works-citing-dew/Artemov_S/`.
  - *p. 8, checked as an image:* eq. (4), PA ⊢ ∀x[p(x):¬x:⊥].
  - *Same page:* the remark that G2 forbids a uniform invariant but not one per
    derivation.
  - *§3.3, same page:* CCon_T := ∀x □_T ¬x:_T ⊥, derivable in PA.
  - *Definitions 1–2 (p. 9):* from the text layer.
- S. Artemov, "Serial properties, selector proofs, and the provability of
  consistency", arXiv:2403.12272. Two pages checked as images:
  - *p. 4, §1.3's definition:* a selector plus a verifier, where selectors are
    explicit primitive recursive operations.
  - *p. 15, §5.2:* the non-contentual selector.
- E. Gadsby, "Properties of selector proofs", arXiv:2509.19373. Pages 3–4, 6–7
  and 9 checked as images:
  - *The setup:* S ⊆ T, both in the language {0, S, +, ·, <}. S is Σ₁-sound,
    contains IΣ₀ + Exp, and proves HBL and strong Σ₁ completeness for □_T.
  - *Definitions 3.1–3.2*, and *Remark 3.3* (implicit selector provability,
    S ⊢ ∀x □_T F(x)).
  - *Example 3.4:* Artemov's result over IΣ₀ + Exp.
  - *Proposition 4.1,* with Ignjatović's corollary.
  - *Remark 4.2:* the selectors of Prop. 4.1 are non-contentual.
- The codification (branch `sjas-codification`):
  - *§3.2, the grounding functions:* non-growth, plus addition and
    Double(x) = x+x on the tableaux line.
  - *§5.1, the axiom groups:* all Π₁-class. Group-1 is finite and proves every
    true Δ₀ sentence. Group-2 is the reflection schema. Group-3 is Π₁-class.
- **Literature.** None of these papers compares selector proofs with Willard's
  systems technically. Artemov 2019 thanks Willard for the Sacks/Gödel
  quotations. Artemov 2024 lists a Willard paper among "further results and
  surveys". Gadsby does not cite Willard.

## The comparison in one paragraph

Artemov changes what consistency means and keeps the system. Willard keeps the
formula and changes the system.

Artemov replaces ∀x ¬x:⊥ by the scheme {¬x:⊥} and proves the scheme in PA with
a selector and a verifier. A self-justifying system postulates ∀x ¬x:⊥ itself
(Group 3), but each of its refutations can only consume Artemov's scheme. The
guard lets a refutation instantiate Group 3 only at values below its own size,
so a refutation touches finitely many members of the scheme.

Artemov locates G2 in the internal quantifier's reach to nonstandard x. Willard
keeps the quantifier and bounds its reach in use.

## 1. Con and CCon change places

Artemov's constructive consistency formula is CCon_T := ∀x □_T ¬x:_T ⊥ (2019,
§3.3). It is the implicit form of a selector proof (Gadsby, Remark 3.3).

| | Con: ∀x ¬x:⊥ | CCon: ∀x □¬x:⊥ |
| --- | --- | --- |
| PA | ⊬ (G2) | ⊢ (Artemov) |
| IS_D(A) | ⊢ (Group 3) | ⊬ (sketch below) |

**Claim.** On the 2005 tableaux line (the U-Grounding language), IS_D(A) does
not prove CCon. This claim and its sketch are mine.

1. If A meets Willard's hypotheses and IS_D(A) is consistent, then every
   axiom of IS_D(A) is true in ℕ. Group 3 is true because the system is
   consistent.
2. The axioms are Π₁-class. Skolemize their bounded existentials by functions
   bounded by terms. Herbrand's theorem then gives any theorem ∀x∃y φ(x,y),
   with φ Δ₀, witnesses y ≤ t(x) for a term t of the language.
3. Every term satisfies t(x) ≤ 2^k (x+1), where k is the term's size. Addition
   and Double at most double a value, and the other six functions are
   non-growth.
4. Any y with y :_{IS} ¬(ẋ:⊥) codes a proof whose formula contains a closed
   term for x. A closed term with k symbols denotes at most 2^k, so the term has
   at least log₂ x symbols. At more than five bits per function symbol
   (`Willard2005`), its code exceeds x⁵.
5. So, if IS_D(A) proved CCon, it would prove ∀x∃y ≤ t(x) [y : ¬(ẋ:⊥)]. By 1
   that bounded theorem is true, and by 3 and 4 it is false for large x. ∎

**Why the places swap.** In theories with formalized Δ₀-completeness, Con
implies CCon. That step is necessitation at a variable,
∀x (¬x:⊥ → □¬ẋ:⊥). It costs the numeral of x, which is the quantity the guard
prices. So the inequality that admits Group 3 is the one that excludes every
internal selector. If the sketch holds, it excludes them for any scheme whose
instances mention their index, not only for consistency.

## 2. The guarded soundness proof is a recursive selector proof

**In the Lean file** ([`2026-10-29-guarded-trust.lean`](2026-10-29-guarded-trust.lean)):
`soundAt n` is the n-th selection ("no proof of size ≤ n proves ⊥"). Lean's
termination check is the verifier. `below`, which is the `con` clause, is the
recursive call.

**In arithmetic (expected, not checked).** PA should selector-prove the
consistency scheme of IS(PA) contentually, by running `Willard2001` Thm 4.3
one refutation at a time. For a refutation r, the selection s(r) does this:

1. It shows that the axioms occurring in r hold below r:
   - Groups 0–1 by PA's proofs of them;
   - each Group-2 instance by cases on its specific y, where a PA proof y of Φ
     is used to derive Φ;
   - Group 3 below r by the selections s(r′) for r′ < r.
2. The guard lemma (Lemmas 4.6–4.8) then gives ¬r:⊥.

PA verifies ∀r[s(r):¬r:⊥] by induction. By Gadsby's Prop. 4.1, after
Ignjatović, this is equivalent to PA ⊢ Con(PA) → Con(IS(PA)).

**The invariants differ, and so do their reasons.**

- *Artemov's invariant* for a derivation d is Tr_{n(d)}. Its strata are by
  formula complexity, forced by Tarski.
- *Willard's invariant* is truth below r. Its strata are by value, forced by
  T: only below r can T's truth come from the induction hypothesis, and the
  guard makes truth below r sufficient.

Both arguments work one derivation at a time, for the reason Artemov gives
(2019, p. 8): G2 "does not rule out the possibility of having an invariant I_S
for each S, but prohibits having such an invariant I uniformly for all
derivations S."

**Gadsby's scope.** His setup needs S ⊆ T, with S ⊇ IΣ₀ + Exp, in the language
of arithmetic. So IS(A) can be the theory W whose consistency is proved, but
not the theory T that proves it. That is consistent with §1.

## 3. Contentuality

- **Artemov's counterexample.** 2024 §5.2 gives a selector that proves the
  scheme but is not a consistency proof: s(n) "only tells us that if n contains
  ⊥, we would still be able to offer a fake proof of ¬n:⊥".
- **IS(A)'s cheaper selector.** Over a base that can build numerals
  (IΣ₀ + Exp), IS(A) has a cheaper selector: instantiate Group 3 at ẋ. It cites
  what is to be proved, so by Artemov's criterion it is not a consistency
  proof. The 2026-10-06 note gave the same verdict on λᶜᵉʳᵗ₀'s H°.
- **What the new framing adds.** It says what the internal axiom is: the
  recursive call of a contentual selector proof. That proof's invariant and
  guard lemma live outside the system, and §1 says they must.

## 4. λᶜᵉʳᵗ₀

- **What the argument rules out.** §1's argument excludes an internal
  selector built from held certificates at a fixed budget, because the
  certificates grow with their index.
- **What it does not touch.** Selectors over free codes, which have System T
  strength. So the 2026-10-06 note's §6 conjecture (selector proofs of BF₀(n))
  is unaffected.

## Status

- **Checked as page images:** the Artemov and Gadsby statements used above.
- **Sketch (mine):** IS_D(A) ⊬ CCon. Steps 2 and 4 are not checked against
  the corpus. The codification states no such result.
- **Expected, not checked:** PA's contentual recursive selector for
  Con(IS(PA)), and PA ⊢ Con(PA) → Con(IS(PA)).
- **Checked in Lean, 2026-10-29:** `soundAt`, the recursive selector of the
  toy syntax.

## Open items

1. Prove IS_D(A) ⊬ CCon. This needs the Herbrand bound for Willard's Π₁-class
   axioms and the numeral-code bound for the 2005 encoding, both checked
   against the codification.
2. Formalize `Willard2001` Thm 4.3 as a PA selector, or prove
   PA ⊢ Con(PA) → Con(IS(PA)) directly.
