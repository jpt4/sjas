# Review of R1, and the resolution of G7

*2026-09-01. Reviewer: the author of R1, after acquiring literature R1 was
written without.*

---

## 1. G7 is resolved, and the answer is that it cannot be acquired

**Gap G7** recorded "Beklemishev 2010 survey (simplified SJAS presentation
claim)" as `refinement-prep`, to be acquired *before* the Refinement began. It
was the Refinement's named inspiration, taken from `prose/sjas_synthesis.txt`:
"taking inspiration from **Lev Beklemishev's claim of a simplified presentation
of SJAS**".

**There is no such survey.** The trail terminates in a footnote.

Fedor Pakhomov, *A weak set theory that proves its own consistency*
(arXiv:1907.00877v2, 27 August 2019), footnote 3:

> "Note that Beklemishev have developed a **still unpublished** simplification
> of Willard's construction (which is different from the approach used in the
> present paper)."

and its Acknowledgments:

> "I am grateful to Lev Beklemishev for introducing me to the Willard's
> approach to construction of theories proving their own consistency³ and for
> latter stimulating discussion of the results of the present paper."

Three further checks:

- Willard's **own** citations to Beklemishev are three technical papers on
  reflection principles (`Willard2011` refs [5]–[7]; `Willard2016`) and, in
  `Willard2020`/`-LFCS`, the 2005 *Russian Mathematical Surveys* survey and the
  2014 APAL paper. **None** is a survey of Willard's systems, and none makes a
  simplification claim.
- The current Wikipedia article *Self-verifying theories* — the likely conduit
  for the synthesis's phrasing — **no longer mentions Beklemishev at all**. An
  earlier revision did, and its wording ("a still unpublished simplification")
  matches Pakhomov's footnote, which is presumably its source.
- No published Beklemishev work on Willard's construction surfaced in search.

**Disposition: G7 closed as `accepted` — unacquirable, not unlocated.** The
simplification is unpublished as of 2019 and no later publication is evident.
Acceptance criterion **B4** ("Beklemishev's claim compared") is therefore
**unmeetable as written** and is replaced (§4).

This matters beyond bookkeeping. R1 was written under the assumption that a
canonical simplification existed and that R1 might be duplicating or diverging
from it. It does not exist in the record. R1's identification stands on its own,
and there is nothing to reconcile it with.

---

## 2. What the acquisition did to R1

Searching for Beklemishev produced something better than the survey: a
**published, peer-reviewed refinement of the SJAS idea by another author**, held
now at `lit/pakhomov2019_weak_set_theory_proves_own_consistency_arxiv_1907.00877.pdf`.

It also falsifies R1's headline claim as stated.

### 2.1 The finding

R1 §1 asserts:

> "A formal system can consistently assert its own consistency **exactly when**
> naming an integer is not cheaper than proving something about it."

The "exactly when" is an **overclaim**, and Pakhomov is the counterexample.

`H_{<ω}` proves its own Hilbert-style consistency. It also **proves the
existence of every hereditarily finite set** — so it is not growth-restricted in
Willard's sense at all. Willard's systems fail to prove *successor*, *addition*
or *multiplication* total; Pakhomov's proves all three and much more. On R1's
criterion as written, `H_{<ω}` should be inconsistent. It is not.

The mechanisms are genuinely different, and Pakhomov says so of Beklemishev's
too ("different from the approach used in the present paper"). There are at
least **three** routes to self-verification on the record, and R1 described one.

### 2.2 What Pakhomov's mechanism actually is

> "in order to prove `Con_pred(H^ω_{<ω})` in `H^ω` it is enough to prove in EA
> that for any Gödel number `p` of a `H^ω_{<ω}`-proof, if `2⁰_p` is defined then
> `p` could not be a proof of contradiction. To prove the latter inside EA we
> **construct a finite model `M` of the size ≤ `2⁰_p` that satisfies all the
> axioms of `H^ω_{<ω}` that occur in `p`**."

The theory is infinitely axiomatizable, and **all its finite subtheories have
finite models**. Any proof uses finitely many axioms; those axioms have a finite
model; so no proof is a refutation. This evades **Pudlák's** strong G2 — which
needs cut-shortening, hence successor totality — by a route orthogonal to
Willard's denial of successor totality.

And it is still bounded: the argument is relativised to the **superexponential
cut**. `H^ω_{<ω}` proves a `Π₁` sentence iff EA proves it holds on that cut. So
there *is* a growth restriction; it simply sits at superexponentiation rather
than at successor.

### 2.3 The repair

The two mechanisms are instances of one shape, and that shape — not the naming
rate — is the essential idea:

> **A system is self-verifying when, for every proof it can write, a bounded
> semantic witness that the proof is not a refutation can be constructed within
> the system's own means.**

- **Willard** constructs a *partial valuation on one tableau branch*: the
  `Positive(a,b)` branch of `Willard2005` Lemmas 1–2, with every parameter
  bounded by `Val(u) ≤ b·2^{Δ(s)}`, and Lemma 2 showing such a branch cannot
  close. "Within its own means" is enforced by the naming rate — the bound must
  be nameable, which is R1's criterion.
- **Pakhomov** constructs a *full finite model of the axioms actually used*,
  bounded by `2⁰_p`. "Within its own means" is enforced by the superexponential
  cut.

Both bound a semantic object by a function of proof length that the system
cannot outrun. R1's naming criterion is the **arithmetic instance** of this, not
the general statement — and R1 should say so.

This is a strengthening, not a retraction: it explains *why* the five dials
agree (§5.1 survives untouched, since all five are Willard-internal), and it
supplies the first external test the criterion has had.

---

## 3. Other review findings on R1

| # | Finding | Disposition |
| --- | --- | --- |
| **1** | §1's "exactly when" is falsified by Pakhomov (§2 above) | **corrected** — claim generalised, Willard's form marked as the arithmetic instance |
| **2** | §8 offers Brown–Palsberg as *the* computational analogue on the strength of structural similarity alone; no formal correspondence is established | **accepted, with reason** — §8 already labels the transfer "a conjecture of this Refinement, not a result". Wording tightened to name it an analogy |
| **3** | §2 says Willard's systems "lose the derivability conditions". Precisely, they fail **D3** (`□φ → □□φ`); D1 and D2 are not the obstruction | **corrected** |
| **4** | §5's table entry for the incremental convention says "too slow to build ℕ", which overstates `Willard2006a` — the convention *does* reach every integer, it is merely not **Continuously Expanding** (`K_i` growth) | **corrected** |
| **5** | Pakhomov's characterisation of Willard — the axioms "are not completely natural in the sense that some of axioms are constructed using **Diagonal Lemma**" — is a substantive external critique R1 does not carry | **corrected** — added to §7's costs |
| **6** | Drift **D81** (`SemPrf^K` strict vs non-strict) was found during R1 and correctly recorded | no action |
| **7** | Beklemishev–Shamkanov 2016, *Some abstract versions of Gödel's second incompleteness theorem based on non-classical logics*, gives an abstract G2 that **fails** for a contraction-free logic — directly relevant to §2's claim about what G2 needs | **carried** — new gap **G38**, acquisition target for R5 |

Sections checked and found sound: §3 (the `ω₁^K(p)` verification cost, verified
against `Willard2007-APAL` §4), §4 (the two sequences and the `Willard2009`
Lemma 5 quote, both image-verified during Codification), §5.1 (the five-dials
argument, which is internal to the corpus and unaffected), §6 (the Rosser
analogue), §9 (the self-limiting statements, all anchored).

---

## 4. Revised acceptance for the stage

**B4 is replaced.** "Compare Beklemishev's claim" is unmeetable — the work is
unpublished. It becomes:

> **B4′** Compare the identification against the published external
> refinements of the SJAS idea. **Pakhomov 2019 — done (§2).** Beklemishev —
> **permanently blocked**, recorded, not pending.

**B5 stands and is now better specified.** Making the criterion a definition
must now cover *both* instances — Willard's bounded valuation and Pakhomov's
bounded finite model — and prove the five dials are instances of the general
form, not merely of the arithmetic one. That is a materially better target than
R1 had.

---

## 5. Sources

- Fedor Pakhomov, *A weak set theory that proves its own consistency*,
  [arXiv:1907.00877](https://arxiv.org/abs/1907.00877) — acquired.
- [Wikipedia, *Self-verifying theories*](https://en.wikipedia.org/wiki/Self_verifying_theories)
  — current revision; no Beklemishev mention.
- L. D. Beklemishev and D. S. Shamkanov, *Some abstract versions of Gödel's
  second incompleteness theorem based on non-classical logics*, in *Liber
  Amicorum Alberti* (College Publications, 2016), pp. 15–29 — **not held**, gap
  **G38**.
