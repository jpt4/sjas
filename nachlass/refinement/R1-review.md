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

An earlier version of this section claimed it falsified R1's headline claim.
It does not; §2.1 retracts that.

### 2.1 The finding — **retracted 2026-09-02**

This section claimed that Pakhomov falsified R1's headline claim, on the ground
that `H_{<ω}` "proves the existence of every hereditarily finite set" and so
"is not growth-restricted in Willard's sense", making it a counterexample to the
naming criterion.

**That reading of Pakhomov is wrong.** He states it in the Introduction —
**"Important restriction here is that both `H` and `H_{<ω}` could not prove
totality of successor function"** — a sentence that straddles the page break,
**pp. 3–4**. A second, narrower statement appears at **p. 22**: "the properties
of SUM in theory `H` that could not even prove totality of successor function",
which is about `H` alone. (An earlier version of this paragraph cited the second
as "p. 21" and read it as a restatement of the first.) `H_{<ω}`
therefore evades Pudlák by **exactly** Willard's route — Pudlák's cut-shortening
technique requires successor totality, which is why Willard denies it and why
Pakhomov must too. Pakhomov's own framing is a *naturalness* claim, not a
mechanism claim:

> "Dan Willard has constructed examples of c.e. arithmetical theories that could
> not prove the totality of successor function but could prove their own
> Hilbert-style consistency. The theories in his examples are not completely
> natural in the sense that some of axioms are constructed using Diagonal Lemma.
> The main result of the present paper is the construction of **a more natural
> example of this kind**."

The earlier claim that his route is "orthogonal to Willard's denial of successor
totality" is the precise opposite of what the source says.

**What survives.** The *generalisation* R1 made in response — from naming rate
to bounded semantic witness — is still the better statement, but for a different
and weaker reason: Pakhomov supplies a **second kind of witness** (a finite model
of the axioms used) alongside Willard's (a bounded valuation on a branch), and a
criterion phrased in terms of the witness covers both. That is a reason to
prefer the general form. It is not a falsification of the special one.

**And the corpus already held the analogue, uncited.** `Willard2006a` Theorem 6
(**`sketch`** — the status belongs here, since this is the comparison the section
turns on) gives **`ISINF(A)`**: *Infinitely Far-Reaching* — some finite subset
proves `∃x Pred^N(x) = 1` for every `N` — while, in Willard's own words,
"Infinitely Far-Reaching **without sustaining an ability to prove successor is a
total function**" (§6, image-verified 2026-09-02). That is
structurally `H_{<ω}`'s shape. Had this review consulted `../codification/registry/systems.md`
before reaching for a falsification, the comparison would have been to `ISINF`
rather than to a misreading.

**Consequences carried through.** `refined-sjas.md` §1 is corrected;
[`R3-the-margin.md`](R3-the-margin.md) §5's classification is corrected; and
ADR-0002's **B4′** stands, since Pakhomov *is* a published external refinement
worth comparing against — the comparison simply comes out differently.

## 3. Other review findings on R1

| # | Finding | Disposition |
| --- | --- | --- |
| **1** | §1's "exactly when" is an overclaim | **corrected** — claim generalised, Willard's form marked as the arithmetic instance. Note the *reason* first given (a Pakhomov counterexample) was withdrawn at §2.1; the generalisation stands on the two-witness argument instead |
| **2** | §8 offers Brown–Palsberg as *the* computational analogue on the strength of structural similarity alone; no formal correspondence is established | **accepted, with reason** — §8 already labels the transfer "a conjecture of this Refinement, not a result". Wording tightened to name it an analogy |
| **3** | §2 says Willard's systems "lose the derivability conditions" without saying which | **superseded 2026-09-02.** This row's own repair — "they fail **D3**; D1 and D2 are not the obstruction" — was itself wrong. `Willard1993-TR` p. 12 names **(2)** for `IS(A)`, as a *uniformity* failure. See `refined-sjas.md` §2.3 |
| **4** | §5's table entry for the incremental convention says "too slow to build ℕ", which overstates `Willard2006a` — the convention *does* reach every integer, it is merely not **Continuously Expanding** (`K_i` growth) | **corrected** |
| **5** | Pakhomov's characterisation of Willard — the axioms "are not completely natural in the sense that some of axioms are constructed using **Diagonal Lemma**" — is a substantive external critique R1 does not carry | **corrected** — added to §7's costs |
| **6** | Drift **D81** (`SemPrf^K` strict vs non-strict) was found during R1 and correctly recorded | no action |
| **7** | Beklemishev–Shamkanov 2016, *Some abstract versions of Gödel's second incompleteness theorem based on non-classical logics*, gives an abstract G2 that **fails** for a contraction-free logic — directly relevant to §2's claim about what G2 needs | **discharged 2026-09-01, not carried.** G38 was raised and closed the same day: the paper was acquired and assessed at [`R2-beklemishev-shamkanov-assessment.md`](R2-beklemishev-shamkanov-assessment.md), and ADR-0002 records R2 as done. This cell said "acquisition target for R5" until 2026-09-02, contradicting two other files in its own commit |

Sections checked and found sound: §3 (the `ω₁^K(p)` verification cost, verified
against `Willard2007-APAL` §4), §4 (the two sequences and the `Willard2009`
Lemma 5 quote, both image-verified during Codification), §6 (the Rosser
analogue). **§9 was wrongly cleared here** — its first paragraph still carried
the stale instruction to acquire G7, which §1 of this very document had closed;
corrected 2026-09-02. §5.1 was cleared as "internal to the corpus and
unaffected", which was true of the dials but did not check §5's criterion
itself; see `R3-the-margin.md` §3.1.

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

*Superseded 2026-09-02.* R3 delivered the definition and answered the dial half
**at three of five**, not five: `ℜ`, `Herb−k` and `Z` are proved to move the
margin, Level(n)'s instance is **retracted**, and Hybrid(H) is unplaced. The
"general form" half was not attempted, because R3 §5 refutes the premise it
rested on — Beklemishev–Shamkanov's system is not self-verifying, so there is no
third instance for a general form to cover. See
[`R3-the-margin.md`](R3-the-margin.md) §6 and ADR-0002's revised **B5**.

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
